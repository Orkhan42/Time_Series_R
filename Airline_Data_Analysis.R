library(data.table)
library(tidyverse)
library(inspectdf)
library(tidymodels)
library(modeltime)
library(lubridate)
library(timetk)
library(rsample)
library(forecast)


data <- fread("AirPassengers.csv")

data %>% glimpse()

data %>% head()
data %>% colnames()

data <- data %>% rename( Passengers = "#Passengers")

data$Month <- paste0(data$Month, '-01') %>% as.Date(.,"%Y-%m-%d")

data %>% inspect_na()

FALSE <- interactive

splits <- initial_time_split(data, prop = 0.8)

data %>% names()

model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.021
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Passengers ~ Month + as.numeric(Month) + factor(month(Month, label = TRUE), ordered = F),
      data = training(splits))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Passengers ~ Month, data = training(splits))

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Passengers ~ Month, data = training(splits))


models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet
)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = F
  )

calibration_tbl <- model_fit_arima_boosted %>%
  modeltime_calibrate(new_data = testing(splits))


calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 21,
    .interactive = T
  ) 

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data)

refit_tbl %>%
  modeltime_forecast(h = "1 year", actual_data = data) %>%
  plot_modeltime_forecast(
    .legend_max_width = 21,
    .interactive = T
  )
