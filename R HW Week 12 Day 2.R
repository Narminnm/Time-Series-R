library(tidyverse)
library(lubridate)
library(timetk)
library(h2o)
library(caTools)
library(highcharter)
library(tidymodels)
library(modeltime)
library(data.table)
library(skimr)
install.packages("zoo")
library(zoo)


df <-  read.csv("AirPassengers (3).csv")
df %>% view()

df %>% skim()
df <- df %>% rename(Date=Month,Passengers=X.Passengers)
df$Date <- paste0(df$Date,"-01") %>% as.Date(df$Date,format="%Y-%m-%d")

df %>% plot_time_series(Date, Passengers)


splits <- initial_time_split(df, prop = 0.8)

# Model 1: arima_boost

model_fit_arima_boosted <- arima_boost() %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Passengers ~ Date, data = training(splits))

# Model 2: ets
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Passengers ~ Date, data = training(splits))


# Model 3: prophet
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Passengers ~ Date, data = training(splits))

#Add fitted models to a Model Table.----

models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet
  )

models_tbl

#Calibrate the model to a testing set.----
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

#Testing Set Forecast & Accuracy Evaluation----
#Visualizing the Forecast Test----
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    #.interactive      = interactive
  )


#Accuracy Metrics----
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    #.interactive = interactive
  )

#Refit to Full Dataset & Forecast Forward----

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = df)

refit_tbl %>%filter(.model_id==1) %>%
  modeltime_forecast(h = "1 years", actual_data = df) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    #.interactive      = interactive
  )







