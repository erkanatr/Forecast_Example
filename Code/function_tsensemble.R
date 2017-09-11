
# Funktion erstellt Forecasts basierend auf Forecast Modellen
ml_forecast_ensemble <- function(df_data) {

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
# -- forecast error
preds <- forecast(model, h = nrow(test))

df_ensemble <- tibble(p_ensembleml = preds)
  return(df_ensemble)
}
