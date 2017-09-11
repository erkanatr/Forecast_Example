# ------------------------------------------------------------------------------
# Transfers a data frame to a ts object.
#
# @param data: The Dataframe
#
# @param value: The column of the value for the timeseries
#
# @param freq: The frequency of the timeseries. The default value is monthly
#              data.
#
# @return ts: A timeseries object.
# ------------------------------------------------------------------------------

df_to_ts <- function(data, value, freq = 12) {
  # -- get the start date of the dataframe
  start = c(year(min(data$Monat)), month(min(data$Monat)))
  # -- get the end date of the dataframe
  end = c(year(max(data$Monat)), month(max(data$Monat)))
  # -- build the timeseries object
  ts <- data %>%
    select_(value) %>%
    unlist(use.names = FALSE) %>%
    ts(frequency = freq, start = start, end = end) 
  return(ts)
}



# ------------------------------------------------------------------------------
# Die Funktion erstellt basierend auf einem Trainingsdatensatz mehrere Fore-
# castingmodelle und erstellt für den übergebenen Testdatensatz mit jedem Modell
# Forecasts. Derzeit werden nur monatliche Daten unterstützt. Das Dataframe 
# muss folgende Struktur haben:
#
#   Date  | y
# --------|-------
# 201606  | 543
#  ...    | ...
#
#
# @param df_current_train: The Dataframe with the train data.
#
# @param df_current_test: The Dataframe with the test data.
#
# @return current_results: Das Dataframe mit den Forecasts für die Testdaten.
# ------------------------------------------------------------------------------

multiple_forecasts <- function(df_current_train, df_current_test) {
  # -- Create all Forecasts from forecast package
  current_results <- df_current_test %>%
    # -- Holt Winters with additive seasonal component --
    #bind_cols(., create_forecast(df_current_train, df_current_test, "hw", 
    #                         list(seasonal = "additive", 
    #                         initial = "optimal"))) %>%
    # -- Auto.Arima -- 
    bind_cols(., create_forecast(df_current_train, df_current_test, 
                             "auto.arima")) %>%
    # -- ARFIMA --
    bind_cols(., create_forecast(df_current_train, df_current_test, "arfima")) %>%
    # -- forecast hybrid -- 
    cbind(., create_forecast(df_current_train, df_current_test, "hybridModel", 
                              list(errorMethod = "RMSE", weights = "insample.errors",verbose = FALSE))) %>%
    # -- TBATS -- 
    bind_cols(., create_forecast(df_current_train, df_current_test, "tbats")) %>%
    # -- ETS -- 
    bind_cols(., create_forecast(df_current_train, df_current_test, "ets", 
                             list(additive.only = TRUE))) %>%
    # -- Bagged ETS -- 
    #cbind(., create_forecast(df_current_train, df_current_test, 
    #                          "baggedETS")) %>%
    # -- xgboost --
    bind_cols(., create_forecast(df_current_train, df_current_test, "xgbar")) %>%
    # -- Neural Network -- 
    bind_cols(., create_forecast(df_current_train, df_current_test, "nnetar", 
                             list(scale.inputs = TRUE, repeats = 1000))) %>%
    # -- Prophet linear--
    bind_cols(., forecast_prophet(df_current_train, "linear")) %>%
    # -- Prophet logistic
    bind_cols(., forecast_prophet(df_current_train, "logistic")) %>%
    # -- es from smooth
    bind_cols(., create_forecast(df_current_train, df_current_test, "es", 
                             list(silent = TRUE)))
  
  # -- Die bisherigen Forecasts werden mit Hilfe des Opera Packages in einem
  # -- Modell kombiniert.
  #forecast_ensemble_results <- forecast_ensemble(current_results)
  
  # -- Kombiniere die Vorhersagen vom Ensemble mit den Vorhersagen der anderen
  # -- Modelle
  #current_results <- bind_cols(current_results, forecast_ensemble_results)
  
  return(current_results)
}


# -- forecast function based on forecast package

create_forecast <- function(train_df, test_df, func, func_args = list()) {
  library(forecast)
  ts_train <- train_df %>%
    df_to_ts("y") 
  
  ts_test <- test_df %>%
    df_to_ts("y")
  
  #-- Data Argument for es from smooth package is called data
  if(func == "es") {
    func_args[["data"]] <- ts_train
  } else {
    func_args[["y"]] <- ts_train
  }
    
  h <- length(ts_test)
  
  method_name <- paste0("p_", func)
    
  # -- gbar and nnetar return slightly different values and therefore are 
  # -- handled in a different manner
    if(func %in% list("xgbar", "nnetar")) 
    {
      pred <- do.call(func, args = func_args) %>%
        forecast(h = h) 
      pred$mean %>% as_tibble() %>%
        select_(.dots=setNames(paste0("x"), method_name)) %>%
        return()
    } 
    else {
      do.call(func, args = func_args) %>%
        forecast(h = h) %>%
        as_tibble() %>%
        select_(.dots=setNames(paste0("`Point Forecast`"), method_name)) %>%
        return()
    }
}

forecast_ensemble <- function(current_results) {
  
  # -- stack all forecasting models
  experts <- current_results %>%
    select(., starts_with("p_")) %>%
    as.matrix()
  
  test_data_ts <- current_results %>%
    df_to_ts("y") 
  
  mix <- mixture(Y = test_data_ts, experts = experts, model = "OGD", 
                 loss.type = "percentage")
  p_mix <- ts(predict(mix, experts, test_data_ts, type='response'), 
              start = c(2015, 07), frequency = 12)
  
  
  ensemble_result <- as_tibble(p_mix) %>%
    rename(p_ensemble = `Series 1`)
  return(ensemble_result)
}

# -- function for creating forecasts with prophet
forecast_prophet <- function(df_train, growth)
{
  name <- paste0("p_prophet_", str_sub(growth, 1L, 3L))
  # -- prepare dataframe for prophet
  df_train <- df_train %>%
    select(ds = Monat, y) 
    #mutate(y = log(y)) # log for not getting negative values
  max_train <- as.Date(END_TRAIN)
  
  if(growth == "logistic") {
    # -- ermittle cap: maxium + 25%
    cap <- max(df_train$y) + max(df_train$y) * 0.25
    df_train <- df_train %>%
      mutate(cap = cap)
  }
  
  # -- train prophet model and predict
  model <- prophet(df_train, growth = growth)
  
  if(growth == "logistic") {
    future <- make_future_dataframe(model, periods = 29, freq = "m") %>%
      mutate(cap = cap)
  } else {
    future <- make_future_dataframe(model, periods = 29, freq = "m") 
  }
  
  forecast <- predict(model, future) %>%
    select(Monat = ds, p_prophet = yhat) %>%
    rename_(.dots = set_names("p_prophet", name)) %>%
    filter(Monat > max_train) %>%
    select_(.dots = list(name)) %>%
    as_tibble()
  return(forecast)
}

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


