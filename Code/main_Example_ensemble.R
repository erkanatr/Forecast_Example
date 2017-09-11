library(tidyverse) # Data manipulation
library(zoo) # class for time series data
library(stringr) # string manipulation
library(lubridate) # manipulating dates
library(forecast) # forecasting library
library(opera) # ensemble technique
library(prophet) # forecasting library from facebook
library(hydroGOF)  # for calculating error metrics
library(forecastxgb) # forecasting with extreme gradient boosting
library(smooth) # forecasting with smoothing techniques
library(timekit) # manipulating timeseries
library(forecastHybrid) # forecasting 
library(foreach) # parralelisation of for loops
library(microbenchmark) # diagnosis tool
library(doParallel) # parralelisation
library(padr) #detect missing values
library(imputeTS) # impute missing values
library(tsoutliers) # identify and replace outliers
library(xts)

set.seed(10) # set random seed for reproducible results

source("Code/func_forecasts.R")
source("Code/func_training.R")

# -- load data
data("AirPassengers")
df_data <- AirPassengers %>%
  tk_tbl %>%
  rename(Monat = index,
         y = value) %>%
  mutate(Monat = as.Date(Monat))

# -- plot data
ggplot(df_data, aes(x = Monat, y = y)) +
  geom_point() + 
  geom_line()

# -- train / test split

number_train <- round(nrow(df_data) * 0.8)

df_train <- rowid_to_column(df_data) %>%
  filter(rowid <= number_train) %>%
  mutate(dataset = "train")

df_test <- rowid_to_column(df_data) %>%
  filter(rowid > number_train) %>%
  mutate(dataset = "test")

END_TRAIN <- max(df_train$Monat)

df_data <- bind_rows(df_train, df_test)

ggplot(df_data, aes(x = Monat, y = y, color = dataset)) +
  geom_line() +
  geom_point()

df_forecasts <- multiple_forecasts(df_train, df_test)

library(tsensembler)

n_lagged_inputs <- 12

train <- df_data %>% 
  select(-rowid, - dataset) %>%
  tk_xts() %>%
  embed_timeseries(n_lagged_inputs) %>%
  as_tibble() %>%
  rownames_to_column(var = "Monat") %>%
  mutate(Monat = as.Date(Monat)) %>%
  left_join(., df_data, by = c("Monat")) %>%
  filter(dataset == "train") %>%
  select(-Monat, -dataset)

nr_val <- 18

train_set <- head(train, nrow(train) - nr_val)
val_set <- tail(train, nr_val)

test <- df_data %>% 
  select(-rowid, - dataset) %>%
  tk_xts() %>%
  embed_timeseries(n_lagged_inputs) %>%
  as_tibble() %>%
  rownames_to_column(var = "Monat") %>%
  mutate(Monat = as.Date(Monat)) %>%
  left_join(., df_data, by = c("Monat")) %>%
  filter(dataset == "test") %>%
  select(-Monat, -dataset)

specs <- model_specs(
  learner = c("bm_ppr","bm_glm","bm_svr","bm_mars"), 
  learner_pars = list(
    bm_glm = list(alpha = c(0, .5, 1)),
    bm_svr = list(kernel = c("rbfdot", "polydot"),
                  C = c(1,3)),
    bm_ppr = list(nterms = 4)
  ))

model <- ADE(target ~., train_set, specs)

# forecast next value and update base and meta models
# every three points;
# in the other points, only the weights are updated
predictions <- numeric(nrow(val_set))
for (i in seq_along(predictions)) {
  predictions[i] <- predict(model, val_set[i, ])@y_hat
  if (i %% 3 == 0) {
    model <-
      update_base_models(model,
                         rbind.data.frame(train, val_set[seq_len(i), ]))
    
    model <- update_ade_meta(model, rbind.data.frame(train, val_set[seq_len(i), ]))
  }
  else
    model <- update_weights(model, val_set[i, ])
}
#preds <- predict(model, val_set)@y_hat

preds <- forecast(model, h = nrow(test))

df_ensemble <- tibble(p_ensemble = preds)

df_forecasts <- select(df_forecasts, -p_ensemble)
df_forecasts <- bind_cols(df_forecasts, df_ensemble)

df_metrics <- calc_metrics(df_forecasts, "mae", mae) %>%
  arrange(value)
df_metrics

best_method <- df_metrics %>%
  group_by(metric) %>%
  summarise(value = min(value)) %>%
  inner_join(., df_metrics) %>%
  select(method) %>%
  pull()

best_method <- str_c("p_", best_method)

df_forecasts %>%
  select(Monat, y = !!best_method) %>%
  mutate(dataset = "forecast") %>%
  bind_rows(., df_data) %>%
  select(-rowid) %>%
  ggplot( aes(x = Monat, y = y, color = dataset)) +
    geom_line() +
    geom_point() +
    ggtitle(str_c("Modell", best_method, sep = " "))


  





