
fill_series_par <- function(df_train, iterate, func, ...) {
  
  start <- min(as.Date(df_train$Monat))
  end <- max(as.Date(df_train$Monat))
  
  used_packages = c("padr", "dplyr", "zoo", "imputeTS")
  
  df_filled <- foreach(i = 1:length(iterate), .combine = bind_rows,
                       .packages = used_packages) %dopar% {
                         
                         temp <- filter(df_train, article_detail == iterate[i]) %>%
                           mutate(filled = "") %>%
                           pad(start_val = start, end_val = end) %>%
                           fill_by_prevalent(Artikel, Ort, article_detail) %>%
                           mutate(filled = if_else(is.na(filled), "filled", "measured")) 
                         
                         replaced_values <- eval(call(func, temp$y))
                         temp %>%
                           mutate(y = replaced_values)
                       }
  return(df_filled)
}


# -- Funktion fügt NA ein für fehlende Zeiten
mark_missing_values <- function(data, iterate) {
  start <- min(as.Date(data$Monat))
  end <- max(as.Date(data$Monat))
  
  used_packages = c("padr", "dplyr", "zoo")
  
  df_marked_missing <- foreach(i = 1:length(iterate), .combine = bind_rows,
                       .packages = used_packages) %dopar% {
                         
                         filter(data, article_detail == iterate[i]) %>%
                           mutate(Monat = as.Date(Monat)) %>%
                           pad(start_val = start, end_val = end) %>%
                           fill_by_prevalent(Artikel, Ort, article_detail)
                       }

  return(df_marked_missing)
}

# -- get articles with missing values
# -- n = korrekte Anzahl in Trainingsdaten
# -- m = minimale Anzahl an Datensätzen
get_articles_missing <- function(data, n = 42, m = 0) {
  articles_missing <- data %>%
    group_by(article_detail) %>%
    summarise(count = n()) %>%
    filter(count < n, count > m) %>%
    ungroup() %>%
    select(article_detail) %>%
    unique() %>%
    pull()
  
  return(articles_missing)
}

# -- Fügt einem Dataframe mit Zeitreihe eine Spalte y_adj hinzu, die korrogierte
# -- Werte für Ausreißer anzeigt

adjust_outliers_par <- function(data, method = "en-masse") {
  
  used_exports <- c("tso")
  
  used_packages <- c("tidyverse", "timekit", "tsoutliers", "zoo")
  
  iterate <- data %>%
    select(article_detail) %>%
    unique() %>%
    pull()
  
  #writeLines(c(""), "log_outliers.txt")
  
  
  df_outliers_adjusted <-
    foreach(i = 1:length(iterate),.combine = bind_rows, .export = used_exports,
            .packages = used_packages, .errorhandling = "remove") %dopar% {
              
              #sink("log_outliers.txt", append = TRUE) # -- log file vorbereiten
              
              # -- Erzeuge das Ausreißer Objekt, hier kann ggf. das genutze Modell 
              # -- getuned werden
              outlier_object <- data %>%
                filter(article_detail == iterate[i]) %>%
                tk_ts() %>%
                #tso(discard.method = method)
                tso(discard.method = method)
              
              # -- die korrigierten Werte werden ausgelesen
              adjusted_outliers <- outlier_object$yadj %>%
                tk_tbl(silent = TRUE)
              
              #sink()
              
              # -- die korrigierten Werte werden dem Datensatz hinzugefügt
              data %>% 
                filter(article_detail == iterate[i]) %>%
                mutate(y_adj = adjusted_outliers$y,
                       Monat = as.Date(Monat)) 
            }
  
  return(df_outliers_adjusted)
}

# -- applies box cox transformation to data and returns the result
apply_box_cox <- function(data) {
  lambda <- BoxCox.lambda(data) # lambda ermittlung ggf. auslagern
  boxcox_trans <- BoxCox(data, lambda)
  return(boxcox_trans)
}

# -- removes the box cox transformation
remove__box_cox <- function(data, lambda) {
  orig_data <- InvBoxCox(Data, lambda)
  return(orig_data)
}