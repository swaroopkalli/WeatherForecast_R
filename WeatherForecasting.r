



#library(ggplot2)
#library(forecast)
#library(readr)

# Load dataset
#data <- read_csv("weather_forecasting_large.csv")

# Check column names to confirm structure
#print(colnames(data))

# Filter data for Seattle
#seattle_data <- subset(data, Location == "Seattle")

# Convert Date column to Date type
#seattle_data$Date <- as.Date(seattle_data$Date)

# Remove any missing values from temperature
# Use backticks for column name with special characters
#seattle_data <- seattle_data[!is.na(seattle_data$`Temperature (°C)`), ]

# Plot Temperature over time
#gplot(seattle_data, aes(x = Date, y = `Temperature (°C)`)) +
 # geom_line(color = "steelblue") +
 # labs(title = "Seattle - Temperature Over Time",
#       x = "Date", y = "Temperature (°C)")

# Create time series object (daily data)
#temperature_ts <- ts(seattle_data$`Temperature (°C)`, frequency = 365)

# Fit ARIMA model
#model <- auto.arima(temperature_ts)

# Forecast next 30 days
#forecast_result <- forecast(model, h = 30)

# Plot the forecast
#plot(forecast_result, main = "Seattle - 30 Day Temperature Forecast")

# Load libraries
library(ggplot2)
library(forecast)
library(readr)
library(lubridate)
library(scales)

# Load dataset
data <- read_csv("weather_forecasting_large.csv")

# Ask user for a city name
city_name <- readline(prompt = "Enter a city name (e.g., Seattle): ")

# Filter data for selected city
city_data <- subset(data, Location == city_name)

# Check if city exists
if (nrow(city_data) == 0) {
  stop("City not found in the dataset.")
}

# Convert Date to Date type
city_data$Date <- as.Date(city_data$Date)

# Remove missing temperature values
city_data <- city_data[!is.na(city_data$`Temperature (°C)`), ]

# Plot temperature over time with months on x-axis
ggplot(city_data, aes(x = Date, y = `Temperature (°C)`)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +  # Show months and years
  labs(title = paste(city_name, "- Temperature Over Time"),
       x = "Date", y = "Temperature (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Tilt x-axis labels

# Create time series object (daily data)
temperature_ts <- ts(city_data$`Temperature (°C)`, frequency = 365)

# Fit ARIMA model
model <- auto.arima(temperature_ts)

# Forecast next 30 days
forecast_result <- forecast(model, h = 30)

# Plot forecast
plot(forecast_result, main = paste(city_name, "- 30 Day Temperature Forecast"))
