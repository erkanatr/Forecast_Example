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

set.seed(10) # set random seed for reproducible results

source("Code/func_forecasts.R")

START_TRAIN <- as.Date("2012-01-01") # Start der Trainingsdaten
END_TRAIN <- as.Date("2015-06-01") # die letzten Daten im Trainingsdatensatz
START_TEST <- as.Date("2015-07-01") # Start des Testzeitraums
END_TEST <- as.Date("2016-06-01") # Ende des Testzeitraumes


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

df_data <- bind_rows(df_train, df_test)

ggplot(df_data, aes(x = Monat, y = y, color = dataset)) +
  geom_line() +
  geom_point()

forecasts <- multiple_forecasts(df_train, df_test)



