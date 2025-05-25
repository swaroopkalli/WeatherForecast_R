library(readr)
library(lubridate)
library(forecast)
library(ggplot2)

# Get user input
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Please provide location and forecast horizon, e.g. Rscript script.R Chicago 30")
}
user_location <- args[1]
forecast_horizon <- as.numeric(args[2])

# Load dataset
data <- read_csv("weather_forecasting_large.csv")

# Clean and prepare data
data$Date <- ymd(data$Date)
names(data)[names(data) == "Temperature (°C)"] <- "Temperature"
names(data)[names(data) == "Humidity (%)"] <- "Humidity"
names(data)[names(data) == "Precipitation (mm)"] <- "Precipitation"

# Filter and clean for the user-specified location
user_data <- subset(data, Location == user_location)
user_daily <- aggregate(cbind(Temperature, Humidity, Precipitation) ~ Date, user_data, mean, na.rm = TRUE)
user_daily <- user_daily[order(user_daily$Date), ]

# Create time series
ts_temp <- ts(user_daily$Temperature, frequency = 365)
ts_humidity <- ts(user_daily$Humidity, frequency = 365)
ts_precip <- ts(user_daily$Precipitation, frequency = 365)

# Forecasts
fc_temp <- forecast(auto.arima(ts_temp), h = forecast_horizon)
fc_humidity <- forecast(auto.arima(ts_humidity), h = forecast_horizon)
fc_precip <- forecast(auto.arima(ts_precip), h = forecast_horizon)

# Forecast dates
future_dates <- seq(max(user_daily$Date) + 1, by = "day", length.out = forecast_horizon)

# Forecast DataFrame
forecast_df <- data.frame(
  Date = as.Date(future_dates),
  Temperature = round(as.numeric(fc_temp$mean), 1),
  Humidity = round(as.numeric(fc_humidity$mean), 1),
  Precipitation = round(as.numeric(fc_precip$mean), 2)
)
write_csv(forecast_df, paste0(forecast_horizon, "_day_weather_forecast_", user_location, ".csv"))

# Plotting function
plot_forecasts <- function(fc_data, label, unit) {
  fc_only_df <- data.frame(
    Date = future_dates,
    Forecast = as.numeric(fc_data$mean),
    Lo80 = fc_data$lower[, 1],
    Hi80 = fc_data$upper[, 1],
    Lo95 = fc_data$lower[, 2],
    Hi95 = fc_data$upper[, 2]
  )

  # Y-axis limits based ONLY on forecast mean values
  y_min <- min(fc_only_df$Forecast, na.rm = TRUE)
  y_max <- max(fc_only_df$Forecast, na.rm = TRUE)
  buffer <- max(0.05 * (y_max - y_min), 0.5)

  # Plot 1: Forecast only with zoomed y-axis on predicted mean
  p1 <- ggplot(fc_only_df, aes(x = Date, y = Forecast)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_ribbon(aes(ymin = Lo95, ymax = Hi95), fill = "skyblue3", alpha = 0.2) +
    geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "skyblue", alpha = 0.3) +
    scale_y_continuous(
      limits = c(y_min - buffer, y_max + buffer),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = paste(label, "-", forecast_horizon, "Day Forecast -", user_location),
      x = "Date",
      y = paste(label, "(", unit, ")")
    ) +
    theme_minimal(base_size = 14)

  # Plot 2: Full historical + forecast plot
  p2 <- autoplot(fc_data) +
    labs(
      title = paste("Historical + Forecast -", label, "-", user_location),
      x = "Time", y = paste(label, "(", unit, ")")
    ) +
    theme_minimal(base_size = 14)

  print(p1)
  print(p2)
}

# Generate plots for each variable
plot_forecasts(fc_temp, "Temperature", "°C")
plot_forecasts(fc_humidity, "Humidity", "%")
plot_forecasts(fc_precip, "Precipitation", "mm")
