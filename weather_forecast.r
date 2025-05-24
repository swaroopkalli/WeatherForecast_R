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
  Date = future_dates,
  Temperature = round(fc_temp$mean, 1),
  Humidity = round(fc_humidity$mean, 1),
  Precipitation = round(fc_precip$mean, 2)
)

# Plot forecasts
ggplot(forecast_df, aes(x = Date, y = Temperature)) +
  geom_line(color = "tomato", linewidth = 1.2) +
  labs(title = "Forecasted Temperature (Next 30 Days)", x = "Date", y = "Temperature (°C)") + # nolint
  scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(forecast_df, aes(x = Date, y = Humidity)) +
  geom_line(color = "skyblue", linewidth = 1.2) +
  labs(title = "Forecasted Humidity (Next 30 Days)", x = "Date", y = "Humidity (%)") + # nolint
  scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(forecast_df, aes(x = Date, y = Precipitation)) +
  geom_line(color = "seagreen", linewidth = 1.2) +
  labs(title = "Forecasted Precipitation (Next 30 Days)", x = "Date", y = "Precipitation (mm)") + # nolint
  scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print and save forecast
print(forecast_df)
autoplot(fc_temp) + ggtitle("30-Day Temperature Forecast (Chicago)")
autoplot(fc_humidity) + ggtitle("30-Day Humidity Forecast (Chicago)")
autoplot(fc_precip) + ggtitle("30-Day Precipitation Forecast (Chicago)")
write_csv(forecast_df, "30_day_weather_forecast.csv")
