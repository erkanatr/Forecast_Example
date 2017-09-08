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

#-- Test log
df_train_log <- mutate(df_train, y = log(y))

df_forecasts_log <- multiple_forecasts(df_train_log, df_test)

df_forecast_log <- df_forecasts_log %>%
  mutate_at(exp, .vars = vars(starts_with("p_")))

df_forecast_log <- mutate(df_forecast_log, p_ensemble_naiv = 0.25 * p_hybridModel +
                                          0.25 * p_prophet_lin +
                                          0.25 * p_prophet_log +
                                          0.25 * p_tbats 
                        )

df_metrics_log <- calc_metrics(df_forecast_log, "mae", mae) %>%
  arrange(value)
 
df_metrics_log

  





