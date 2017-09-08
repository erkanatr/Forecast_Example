

apply_forecasts_parallel <- function(df_train, df_test) {
  iterate <- df_train %>%
    select(article_detail) %>%
    unique() %>%
    pull()
  writeLines(c(""), "log.txt")


  # -- Preallocate Memory!
  df_results <- tibble()
  anzahl_forecasts <- length(iterate)
  count <- 0

  used_exports <- c(
    "multiple_forecasts", "df_train", "df_test",
    "iterate", "create_forecast", "df_to_ts", "count",
    "year", "month", "hybridModel", "forecast_prophet",
    "forecast_ensemble", "END_TRAIN"
  )

  used_packages <- c(
    "tidyverse", "timekit", "forecast", "forecastxgb", "zoo",
    "smooth", "prophet", "stringr", "opera"
  )


  df_results <-

    foreach(
      i = 1:length(iterate), .combine = bind_rows, .export = used_exports,
      .packages = used_packages, .errorhandling = "remove"
    ) %dopar% {
      current_article <- iterate[i]

      sink("log.txt", append = TRUE) # -- log file vorbereiten

      print(str_c("current article:", current_article, sep = " "))
      print(paste(count + 1, paste(" von ", anzahl_forecasts)))

      sink() #  um zu verhindern, dass sink stack voll läuft

      df_current_train <- df_train %>%
        filter(article_detail == current_article)

      df_current_test <- df_test %>%
        filter(article_detail == current_article)

      count <- count + 1

      multiple_forecasts(df_current_train, df_current_test)
    }
  return(df_results)
}

run_benchmark <- function(df_train, df_test) {
  res_benchmark <- microbenchmark(
    forpar = apply_forecasts_parallel(df_train, df_test),
    nomral = apply_forecasts(df_train, df_test) %>%
      # -- convert the date to a convenient monthly data format
      mutate(Monat = yearmon(Monat)),
    times = 1L
  )
  return(res_benchmark)
}

abs_error <- function(y_hat, y) {
  return(abs(y - y_hat))
}


# -- function for calculating rmse
calc_metrics <- function(results, name, func) {

        

          col_names <- results %>%
            select(., starts_with("p_")) %>%
            colnames() %>%
            str_replace("p_", str_c(name, "_"))

          forecasts <- results %>%
            select(-Monat) %>%
            select(., starts_with("p_"))

          actual <- results %>%
            select(-Monat) %>%
            select(y)

          results_error <- map2(actual, forecasts, func) %>%
            unlist() %>%
            as.tibble() %>%
            mutate(method = col_names) %>%
            spread(method, value) %>%
        

    gather(key = method, value = value) %>%
    separate(method, into = c("metric", "method"))

  return(results_error)
}

setup_cluster <- function(backend = "doParallel", cores = 4) {
  if (backend == "doParallel") {
    # -- Setup a local doParallel cluster
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    return(cl)
  }

  if (backend == "doAzureParallel") {
    # -- Setup a cloud azure cluster
    # -- TODO
    # 1. Generate your credentials config and fill it out with your Azure information
    # generateCredentialsConfig(“credentials.json”)

    # 2. Set your credentials
    # setCredentials(“credentials.json”)

    # 3. Generate your cluster config to customize your cluster
    # generateClusterConfig(“cluster.json”)

    # 4. Create your cluster in Azure passing, it your cluster config file.
    # cluster <- makeCluster(“cluster.json”)

    # 5. Register the cluster as your parallel backend
    # registerDoAzureParallel(cluster)
    return(5L)
  }
}
