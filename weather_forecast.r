library(readr)
library(lubridate)
library(forecast)
library(ggplot2)
library(dplyr)

# Load dataset
data <- read_csv("weather_forecasting_large.csv")

# Clean and prepare data
data$Date <- ymd(data$Date)
names(data)[names(data) == "Temperature (°C)"] <- "Temperature"
names(data)[names(data) == "Humidity (%)"] <- "Humidity"
names(data)[names(data) == "Precipitation (mm)"] <- "Precipitation"

# Aggregate daily mean for each city
data_daily <- data %>%
  group_by(Location, Date) %>%
  summarise(
    Temperature = mean(Temperature, na.rm = TRUE),
    Humidity = mean(Humidity, na.rm = TRUE),
    Precipitation = mean(Precipitation, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Location, Date)

# Forecasting function
forecast_weather <- function(city_data) {
  ts_temp <- ts(city_data$Temperature, frequency = 365)
  ts_humidity <- ts(city_data$Humidity, frequency = 365)
  ts_precip <- ts(city_data$Precipitation, frequency = 365)

  # ARIMA for temperature and humidity, TBATS for precipitation
  fc_temp <- forecast(auto.arima(ts_temp), h = 30)
  fc_humidity <- forecast(auto.arima(ts_humidity), h = 30)
  model_tbats <- tbats(ts_precip)
  fc_precip <- forecast(model_tbats, h = 30)

  return(data.frame(
    Date = as.Date(seq(max(city_data$Date) + 1, by = "day", length.out = 30)),
    Temperature = round(as.numeric(fc_temp$mean), 1),
    Humidity = round(as.numeric(fc_humidity$mean), 1),
    Precipitation = round(as.numeric(fc_precip$mean), 2),
    Location = city_data$Location[1]
  ))
}

# Apply forecasting function to each city
cities <- unique(data$Location)
forecast_results <- do.call(rbind, lapply(cities, function(city) {
  city_data <- filter(data_daily, Location == city)
  forecast_weather(city_data)
}))

# Save forecast to CSV
write_csv(forecast_results, "30_day_weather_forecast_multiple_cities.csv")

# Plot forecasts for each city
for (city in unique(forecast_results$Location)) {
  city_forecast <- filter(forecast_results, Location == city)

  p1 <- ggplot(city_forecast, aes(x = Date, y = Temperature)) +
    geom_line(color = "tomato") +
    ggtitle(paste("30-Day Temperature Forecast -", city)) +
    theme_minimal()

  p2 <- ggplot(city_forecast, aes(x = Date, y = Humidity)) +
    geom_line(color = "steelblue") +
    ggtitle(paste("30-Day Humidity Forecast -", city)) +
    theme_minimal()

  p3 <- ggplot(city_forecast, aes(x = Date, y = Precipitation)) +
    geom_line(color = "seagreen") +
    ggtitle(paste("30-Day Precipitation Forecast -", city)) +
    theme_minimal()

  print(p1)
  print(p2)
  print(p3)
}