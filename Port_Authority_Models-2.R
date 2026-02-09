# Load libraries
library(readxl)
library(tidyverse)
library(forecast)
library(lubridate)
library(scales)
library(dplyr)
library(ggplot2)
library(car)        # for VIF
library(ggpubr)     # for residual plots
library(factoextra)

#Time Series Forecasting Arima Model 

# STEP 1: Load the dataset

port_data <- read_excel("Port_Authority_Consolidated_Dataset.xlsx", sheet = "Buses_Passengers_Weather")

# STEP 2: Convert Date column to Date type
port_data$Date <- as.Date(port_data$Start_Date)

# STEP 3: Aggregate monthly passenger count 
monthly_data <- port_data %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Month) %>%
  summarise(Passenger_Count = sum(Total_Passengers, na.rm = TRUE)) %>%
  arrange(Month)

# STEP 4: Create time series object from Jan 2020
ts_data <- ts(monthly_data$Passenger_Count, frequency = 12, start = c(2020, 1))

# STEP 5: Fit ARIMA model
model <- auto.arima(ts_data)

# STEP 6: Forecast from june 2024 to dec 2030 (76 months)
forecast_result <- forecast(model, h = 76)

# STEP 7: Create date sequence for both actual and forecast
historical_df <- monthly_data %>%
  rename(Date = Month, Value = Passenger_Count) %>%
  mutate(Type = "Actual")

forecast_dates <- seq(from = as.Date("2024-06-01"), by = "month", length.out = 76)

forecast_df <- data.frame(
  Date = forecast_dates,
  Value = as.numeric(forecast_result$mean),
  Lower_80 = as.numeric(forecast_result$lower[, 1]),
  Upper_80 = as.numeric(forecast_result$upper[, 1]),
  Lower_95 = as.numeric(forecast_result$lower[, 2]),
  Upper_95 = as.numeric(forecast_result$upper[, 2]),
  Type = "Forecast"
)

# STEP 8: Combine historical and forecast data
combined_df <- bind_rows(
  historical_df %>% select(Date, Value, Type),
  forecast_df %>% select(Date, Value, Type)
)

# STEP 9: Plot with date-wise x-axis
ggplot() +
  geom_line(data = combined_df, aes(x = Date, y = Value, color = Type), size = 1) +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_95, ymax = Upper_95), 
              fill = "lightblue", alpha = 0.3) +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_80, ymax = Upper_80), 
              fill = "skyblue", alpha = 0.4) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 months") +
  labs(
    title = "Passenger Count Forecast (2020–2030)",
    x = "Date",
    y = "Passenger Count",
    color = "Data Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#---------------------------------------------------------------------------------------

#Multiple Linear Regression Model


# Load data
data <- read_excel("Port_Authority_Consolidated_Dataset.xlsx", sheet = "Buses_Passengers_Weather")

# View structure
str(data)

# Clean data: remove rows with missing values in key variables
clean_data <- data %>%
  filter(!is.na(Total_Passengers),
         !is.na(MAX_Temp),
         !is.na(SNWD),
         !is.na(PRCP))

# Build Multiple Linear Regression model
model <- lm(Total_Passengers ~ MAX_Temp + SNOW + SNWD + PRCP + AWND, data = clean_data)

# Print model summary
summary(model)



#--------------------------------------------------------------------------------------------

#Clustering Model

# Read the datasets
bus_departures <- read_excel("Port_Authority_Consolidated_Dataset.xlsx", sheet = "Total_Bus_departures")
passenger_departures <- read_excel("Port_Authority_Consolidated_Dataset.xlsx", sheet = "Total_Passenger_Departures")

# Step 1: Data Cleaning
# Drop unnecessary columns
bus_clean <- bus_departures %>%
  select(-c(Start_Date, End_Date, Date_Range, Total_Buses))

pass_clean <- passenger_departures %>%
  select(-c(Start_Date, End_Date, Date_Range, Total_Passengers))

# Step 2: Aggregate data - Compute averages for each carrier
avg_buses <- colMeans(bus_clean, na.rm = TRUE)
avg_passengers <- colMeans(pass_clean, na.rm = TRUE)

# Combine the data into a summary dataframe
carrier_summary <- data.frame(
  Carrier = names(avg_buses),
  Avg_Buses = avg_buses,
  Avg_Passengers = avg_passengers
)

# Step 3: Feature Engineering
# Handle zero Avg_Buses to avoid division errors
carrier_summary <- carrier_summary %>%
  mutate(Avg_Buses = ifelse(Avg_Buses == 0, 0.001, Avg_Buses)) %>%  # Replace zeroes
  mutate(Passengers_per_Bus = Avg_Passengers / Avg_Buses)           # Compute efficiency

# Step 4: Data Validation
# Print summary statistics to ensure no NA/Inf values
print(summary(carrier_summary))

# Step 5: Feature Scaling
scaled_features <- scale(carrier_summary[, c("Avg_Buses", "Avg_Passengers", "Passengers_per_Bus")])

# Ensure no NA/Inf values in scaled data
print(summary(scaled_features))

# Step 6: Optimal Cluster Determination (Elbow Method)
fviz_nbclust(scaled_features, kmeans, method = "wss") +
  labs(title = "Elbow Method - Optimal Clusters for Carriers")

# Step 7: Apply K-means Clustering
# Assuming k = 3 (replace with optimal k from the elbow plot)
set.seed(123)
kmeans_result <- kmeans(scaled_features, centers = 3, nstart = 25)

# Step 8: Add Cluster Assignments
carrier_summary$Cluster <- factor(kmeans_result$cluster)

# Step 9: Visualize Clusters
fviz_cluster(kmeans_result, data = scaled_features,
             geom = "point", ellipse.type = "norm",
             labelsize = 10, main = "Carrier Clusters")

# Step 10: Export or View Final Results
# Print the carrier summary with clusters
print(carrier_summary)


