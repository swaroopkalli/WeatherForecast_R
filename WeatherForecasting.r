
library(ggplot2)
library(forecast)
library(readr)

# Load dataset
data <- read_csv("weather_forecasting_large.csv")

# Check column names to confirm structure
print(colnames(data))

# Filter data for Seattle
seattle_data <- subset(data, Location == "Seattle")

# Convert Date column to Date type
seattle_data$Date <- as.Date(seattle_data$Date)

print(unique(data$Location))

# Remove any missing values from temperature
# Use backticks for column name with special characters
seattle_data <- seattle_data[!is.na(seattle_data$`Temperature (°C)`), ]

# Plot Temperature over time
ggplot(seattle_data, aes(x = Date, y = `Temperature (°C)`)) +
  geom_line(color = "steelblue") +
  labs(title = "Seattle - Temperature Over Time",
       x = "Date", y = "Temperature (°C)")

# Create time series object (daily data)
temperature_ts <- ts(seattle_data$`Temperature (°C)`, frequency = 365)

# Fit ARIMA model
model <- auto.arima(temperature_ts)

# Forecast next 30 days
forecast_result <- forecast(model, h = 30)

# Convert forecast to data frame
forecast_df <- data.frame(
  Date = seq(
    from = max(seattle_data$Date) + 1,
    by = "day",
    length.out = 30
  ),
  Forecast = as.numeric(forecast_result$mean)
)

# Plot only the forecasted temperature
ggplot(forecast_df, aes(x = Date, y = Forecast)) +
  geom_line(color = "tomato", linewidth = 1.2) +
  labs(title = "Seattle - Forecasted Temperature (Next 30 Days)",
       x = "Date", y = "Predicted Temperature (°C)") +
  scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
