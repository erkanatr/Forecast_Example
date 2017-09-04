# -- Hole die Forecasts des jeweils besten Modells
df_best_forecast <- df_forecasts %>%
  gather(key = method,  value = y_hat, starts_with("p_")) %>%
  mutate(method = str_replace(method, "p_", "")) %>%
  inner_join(df_best_method, by = c("method", "Artikel", "Ort")) %>%
  left_join(df_board_forecasts, by = c("Artikel", "Ort", "Monat")) %>%
  mutate(article_detail = article_detail.x) %>%
  select(-article_detail.x, -article_detail.y) %>%
  select(-metric, -value) %>%
  rename(y_board = p_Board_Forecast)

# Spalte y_board hinzufügen

library(DBI)

# -- TODO Secret Package
# -- For Using the SQL Server you need a Database called SDG_Forecasts

# uses the odbc connection MSSQL
con <- dbConnect(odbc::odbc(), "MSSQL", database = "SDG_Forecasts")

DBI::dbWriteTable(con, "T_FAKT_Forecasts", df_best_forecast, append = TRUE)


