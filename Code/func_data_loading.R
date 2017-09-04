



# ------------------------------------------------------------------------------
# Funktion lädt die historischen Daten aus Board und bereinigt sie.
#
# @parm max_date: Das maximal zu ladende Datum
#
# @return Tibble: Die bereinigten Daten. 
# ------------------------------------------------------------------------------

load_hist_data <- function(max_date) {
  hist_data <- read_tsv("Data/hist_data.csv", col_names = TRUE) %>%  
    # -- Rename Columns
    rename(Artikel = `FC Artikel`, 
           Ort = `Inland / Ausland`, 
           y = `[BP-TR] Y(t) historische Werte`) %>% 
    # -- Reorder Columns
    select(Monat, Artikel, Ort, y)  %>% 
    mutate(Monat = str_c(as.character(Monat), "01")) %>%
    # -- Cast to Date Object
    mutate(Monat = as.Date(Monat, format = "%Y%m%d")) %>%
    # -- Cast Artikel to Character
    mutate(Artikel = as.character(Artikel)) %>%
    # -- Remove Entries with missing places and non relevant dates
    filter(Ort != "-1", Monat <= max_date) %>%  
    # -- sort by Date 
    arrange(Monat)                                       
  return (hist_data)
}

# ------------------------------------------------------------------------------
# Funktion lädt BOARD Forecasts. Dazu werden zwei Dateien benötigt:
# 1. Datei mit den Board Forecasts 2. Datei mit Info, welches Modell 
# das beste ist.
#
# @return Tibble: Die bereinigten Board Forecasts. 
# ------------------------------------------------------------------------------

load_board_forecasts <- function() {
  
  # -- Lade Board Forecasts mit allen Modellen
  df_board_forecasts <- read_tsv("Data/board_forecasts.csv", 
                                 col_names = TRUE) %>%
    # -- Rename Columns
    rename(Artikel = `FC Artikel`, 
           Ort = `Inland / Ausland`, 
           model = `FC Model`,
           p_Board_Forecast = `[BP-TR] Forecast`) %>%
    # -- Convert the Board forecast to a valid decimal
    mutate(p_Board_Forecast = as.numeric(str_replace(p_Board_Forecast, ",", "."))) %>%
    # -- build valid Date string
    mutate(Monat = str_c(as.character(Monat), "01", sep = "")) %>%
    mutate(Monat = as.Date(Monat, format = "%Y%m%d"))
  
  # -- Lade infos welches Model das Beste ist
  df_best_model <- read_tsv("Data/board_best_model.csv", col_names = TRUE) %>%
    # -- Rename Columns
    rename(Artikel = `FC Artikel`, 
           Ort = `Inland / Ausland`, 
           bestes_model = `[BP-TR] Bestes Modell Text`)
  
  # -- Um für den BOARD Forecast immer das beste Modell wählen zu können, müssen
  # -- wir die textuelle Beschreibung durch die Codes ersetzen, denn der Code
  # -- ist in der Tabelle mit den Board Forecasts enthalten.
  df_best_model$model <- df_best_model$bestes_model %>%
    # -- replace the desciption with the Code 
    recode("Holt" = "02", 
           "Winters" = "03", 
           "Winters Plus" = "04", 
           "HW mit IM-Korr." = "05", 
           "HW mit Fehlerkorr." = "06", 
           "GL-DS VJ" = "08", 
           "GL-DS" = "09", 
           "Vorjahr" = "10") 
    # -- remove the temporary column
    df_best_model$bestes_model <- NULL
  
  # -- Kombiniere den besten Forecasts mit den eigentlichen Forecasts, um 
  # -- für jeden Artikel nur das beste BOARD Forecastmodell zu erhalten. In der
  # -- ursprünglichen Datei sind nämlich Forecasts vons allen BOARD Modellen 
  # -- enthalten.
  df_board_forecasts <- inner_join(df_best_model, df_board_forecasts, 
                                   by = c("Artikel","Ort", "model")) %>%
    mutate(Artikel = as.character(Artikel)) %>%
    select(-model)
  
  # -- delete the temp object
  rm(df_best_model)
  return(df_board_forecasts)
}

# ------------------------------------------------------------------------------
# Die Funktion erstellt einen beispiels Datensatzt mit einem Artikel
#
# @parm data: Der Trainingsdatensatz
#
# @return Tibble: Der Beispieldatensatz
# ------------------------------------------------------------------------------

forecast_example <- function(data, n) {
  
  article_list <- data %>%
    select(Artikel) %>%
    unique() %>%
    head(n) %>%
    pull()
  
  example_data <- data %>%
    filter(Artikel %in% article_list)
  return(example_data)
}


  
