library(readr)
library(lubridate)
library(forecast)
library(ggplot2)
  
# Load dataset
data <- read_csv("weather_forecasting_large.csv")

# Clean and prepare data
data$Date <- ymd(data$Date)
names(data)[names(data) == "Temperature (°C)"] <- "Temperature"
names(data)[names(data) == "Humidity (%)"] <- "Humidity"
names(data)[names(data) == "Precipitation (mm)"] <- "Precipitation"

# User input of cities
print("Available cities: ")
print(unique(data$Location))
city_name <- input(prompt("Enter name of city of whose you want to see forecast for next 30 days: ")) # nolint

# Filter for Chicago and remove NA
chicago <- subset(data, Location == "Chicago")

# Aggregate daily mean
chicago_daily <- aggregate(cbind(Temperature, Humidity, Precipitation) ~ Date, chicago, mean, na.rm = TRUE) # nolint
chicago_daily <- chicago_daily[order(chicago_daily$Date), ]

# Create time series
ts_temp <- ts(chicago_daily$Temperature, frequency = 365)
ts_humidity <- ts(chicago_daily$Humidity, frequency = 365)
ts_precip <- ts(chicago_daily$Precipitation, frequency = 365)

# Fit ARIMA and forecast 30 days
fc_temp <- forecast(auto.arima(ts_temp), h = 30)
fc_humidity <- forecast(auto.arima(ts_humidity), h = 30)
fc_precip <- forecast(auto.arima(ts_precip), h = 30)

# Forecast dates
future_dates <- seq(max(chicago_daily$Date) + 1, by = "day", length.out = 30)

# Forecast DataFrame
forecast_df <- data.frame(
  Date = as.Date(future_dates), # Ensure Date is Date type for write_csv
  Temperature = round(as.numeric(fc_temp$mean), 1),
  Humidity = round(as.numeric(fc_humidity$mean), 1),
  Precipitation = round(as.numeric(fc_precip$mean), 2)
)

# Save forecast to CSV only (no plots or console output)
write_csv(forecast_df, "30_day_weather_forecast.csv")
