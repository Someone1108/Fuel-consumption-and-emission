# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the datasets
planes_data <- read_csv("Final_plane_emmision_cost_efficiency.csv")
cars_data <- read_csv("FINAL_Updated_Normalized_Car_Fuel_Data_with_Car_Cost.csv")

# Filter and sort top 5 planes based on Travel Cost and Fuel Consumption
top_planes <- planes_data %>%
  filter(!is.na(`Fuel Consumption, SAR (kg/km/seat)`) & !is.na(`Travel Cost by Airplane (AUD)`)) %>%
  arrange(`Travel Cost by Airplane (AUD)`, `Fuel Consumption, SAR (kg/km/seat)`) %>%
  head(50) %>%
  mutate(Type = "Plane", 
         Cost = `Travel Cost by Airplane (AUD)`, 
         FuelConsumption = `Fuel Consumption, SAR (kg/km/seat)`, 
         Name = `Aircraft Type`)

# Filter and sort top 5 cars based on Car Cost and Fuel Consumption
top_cars <- cars_data %>%
  filter(!is.na(`Comb (kg/km/seat)`) & !is.na(`Car Cost per Seat (AUD)`)) %>%
  arrange(`Car Cost per Seat (AUD)`, `Comb (kg/km/seat)`) %>%
  head(50) %>%
  mutate(Type = "Car", 
         Cost = `Car Cost per Seat (AUD)`, 
         FuelConsumption = `Comb (kg/km/seat)`, 
         Name = Model)

# Combine planes and cars datasets
combined_data <- bind_rows(top_planes, top_cars)

ggplot(combined_data, aes(x = FuelConsumption, y = Cost, color = Type)) +
  geom_point(size = 3) +
  labs(title = "Cost vs Fuel Consumption for Planes and Cars",
       x = "Fuel Consumption (kg/km)",
       y = "Cost/seat",
       color = "Vehicle Type") +
  theme_minimal()

