# Load required libraries
library(readr)
library(lubridate)
library(forecast)
library(ggplot2)

# Load the dataset
weather <- read_csv("weather_forecasting_large.csv")

# Convert Date and rename columns
weather$Date <- ymd(weather$Date)
names(weather)[names(weather) == "Temperature (°C)"] <- "Temperature"
names(weather)[names(weather) == "Humidity (%)"] <- "Humidity"
names(weather)[names(weather) == "Precipitation (mm)"] <- "Precipitation"

# Filter for Chicago
city_data <- weather[weather$Location == "Chicago", ]

# Aggregate: group by Date and take mean for each column
city_data_agg <- aggregate(
  cbind(Temperature, Humidity, Precipitation) ~ Date,
  data = city_data,
  FUN = function(x) mean(x, na.rm = TRUE)
)

# Order by date
city_data_agg <- city_data_agg[order(city_data_agg$Date), ]

# Convert columns to time series
ts_temp <- ts(city_data_agg$Temperature, frequency = 365)
ts_humidity <- ts(city_data_agg$Humidity, frequency = 365)
ts_precip <- ts(city_data_agg$Precipitation, frequency = 365)

# Set forecast horizon
forecast_horizon <- 30

# Fit ARIMA models and forecast
model_temp <- auto.arima(ts_temp)
model_humidity <- auto.arima(ts_humidity)
model_precip <- auto.arima(ts_precip)

fc_temp <- forecast(model_temp, h = forecast_horizon)
fc_humidity <- forecast(model_humidity, h = forecast_horizon)
fc_precip <- forecast(model_precip, h = forecast_horizon)

# Create date sequence for forecast
forecast_dates <- seq(
  from = max(city_data_agg$Date) + 1,
  by = "day",
  length.out = forecast_horizon
)

# Create final forecast data frame
forecast_df <- data.frame(
  Date = forecast_dates,
  Temperature = round(fc_temp$mean, 1),
  Humidity = round(fc_humidity$mean, 1),
  Precipitation = round(fc_precip$mean, 2)
)

# Print forecast table
print(forecast_df)

# Plot forecasts
autoplot(fc_temp) + ggtitle("30-Day Temperature Forecast (Chicago)")
autoplot(fc_humidity) + ggtitle("30-Day Humidity Forecast (Chicago)")
autoplot(fc_precip) + ggtitle("30-Day Precipitation Forecast (Chicago)")

# Save to CSV
write_csv(forecast_df, "30_day_weather_forecast.csv")
