# Libraries
library(readr)
library(lubridate)
library(forecast)
library(ggplot2)
library(GGally)
library(corrplot)
library(dplyr)
library(tidyr)
library(patchwork)

# Load dataset
data <- read_csv("weather_forecasting_large.csv")

# Clean and prepare data
data$Date <- ymd(data$Date)
names(data)[names(data) == "Temperature (°C)"] <- "Temperature"
names(data)[names(data) == "Humidity (%)"] <- "Humidity"
names(data)[names(data) == "Precipitation (mm)"] <- "Precipitation"

# Filter for Chicago and prepare data
chicago <- subset(data, Location == "Chicago")
chicago_daily <- chicago %>%
  group_by(Date) %>%
  summarize(across(c(Temperature, Humidity, Precipitation), mean, na.rm = TRUE)) %>%
  arrange(Date)

# Time Series Plots
p1 <- ggplot(chicago_daily, aes(x = Date, y = Temperature)) + geom_line() + ggtitle("Temperature Over Time")
p2 <- ggplot(chicago_daily, aes(x = Date, y = Humidity)) + geom_line() + ggtitle("Humidity Over Time")
p3 <- ggplot(chicago_daily, aes(x = Date, y = Precipitation)) + geom_line() + ggtitle("Precipitation Over Time")

# Monthly Trends
chicago_daily$Month <- month(chicago_daily$Date, label = TRUE)
monthly_avg <- chicago_daily %>%
  group_by(Month) %>%
  summarize(across(c(Temperature, Humidity, Precipitation), mean))

p4 <- ggplot(monthly_avg, aes(x = Month, y = Temperature)) + geom_bar(stat = "identity") + ggtitle("Monthly Avg Temperature")
p5 <- ggplot(monthly_avg, aes(x = Month, y = Humidity)) + geom_bar(stat = "identity") + ggtitle("Monthly Avg Humidity")
p6 <- ggplot(monthly_avg, aes(x = Month, y = Precipitation)) + geom_bar(stat = "identity") + ggtitle("Monthly Avg Precipitation")

# Distribution Plots
p7 <- ggplot(chicago_daily, aes(x = Temperature)) + geom_histogram(bins = 30, fill = "steelblue") + ggtitle("Temperature Distribution")
p8 <- ggplot(chicago_daily, aes(x = Humidity)) + geom_histogram(bins = 30, fill = "darkgreen") + ggtitle("Humidity Distribution")
p9 <- ggplot(chicago_daily, aes(x = Precipitation)) + geom_histogram(bins = 30, fill = "purple") + ggtitle("Precipitation Distribution")

# Temperature Boxplot
p10 <- ggplot(chicago_daily, aes(x = Month, y = Temperature)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Temperature by Month in Chicago", x = "Month", y = "Temperature (°C)") +
  theme_minimal()

# Humidity Boxplot
p11 <- ggplot(chicago_daily, aes(x = Month, y = Humidity)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Humidity by Month in Chicago", x = "Month", y = "Humidity (%)") +
  theme_minimal()

# Precipitation Boxplot
p12 <- ggplot(chicago_daily, aes(x = Month, y = Precipitation)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Precipitation by Month in Chicago", x = "Month", y = "Precipitation (mm)") +
  theme_minimal()

# Correlation Heatmap
corr_matrix <- cor(chicago_daily %>% select(Temperature, Humidity, Precipitation), use = "complete.obs")
corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.col = "black", title = "Correlation Heatmap", mar = c(0,0,1,0))

# Forecast Plots
ts_temp <- ts(chicago_daily$Temperature, frequency = 365)
ts_humidity <- ts(chicago_daily$Humidity, frequency = 365)
ts_precip <- ts(chicago_daily$Precipitation, frequency = 365)

fc_temp <- forecast(auto.arima(ts_temp), h = 30)
fc_humidity <- forecast(auto.arima(ts_humidity), h = 30)
fc_precip <- forecast(tbats(ts_precip), h = 30)

autoplot(fc_temp) + ggtitle("30-Day Temperature Forecast")
autoplot(fc_humidity) + ggtitle("30-Day Humidity Forecast")
autoplot(fc_precip) + ggtitle("30-Day Precipitation Forecast")

# Residual Diagnostics
checkresiduals(fc_temp)
checkresiduals(fc_humidity)
checkresiduals(fc_precip)

# Pairplot for Relationships
ggpairs(chicago_daily %>% select(Temperature, Humidity, Precipitation))

# You can also combine plots using patchwork
(p1 / p2 / p3)
(p4 / p5 / p6)
(p7 / p8 / p9)
(p10 / p11 / p12)

# Create future dates for the next 30 days
future_dates <- seq(max(chicago_daily$Date) + 1, by = "day", length.out = 30)

# Create a data frame with the forecasts
forecast_df <- data.frame(
  Date = future_dates,
  Temperature = as.numeric(fc_temp$mean),
  Humidity = as.numeric(fc_humidity$mean),
  Precipitation = as.numeric(fc_precip$mean)
)

# Write the forecast to a CSV file
write.csv(forecast_df, "30_day_weather_forecast.csv", row.names = FALSE)