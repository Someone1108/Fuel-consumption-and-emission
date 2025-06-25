library(ggplot2)
library(dplyr)
library(readr)

# Load the datasets
planes_data <- read_csv("Final_plane_emmision_cost_efficiency.csv")
cars_data <- read_csv("FINAL_Updated_Normalized_Car_Fuel_Data_with_Car_Cost.csv")

# Select the top 4 most efficient planes based on fuel consumption and emissions
top_planes <- planes_data %>%
  filter(!is.na(`Fuel Consumption, SAR (kg/km/seat)`) & !is.na(`Fuel Consumption, EEA Master Emission Calculator (kg/km/seat)`) & !is.na(`Travel Cost by Airplane (AUD)`)) %>%
  select(`Aircraft Type`, `Fuel Consumption, SAR (kg/km/seat)`, `Fuel Consumption, EEA Master Emission Calculator (kg/km/seat)`, `Travel Cost by Airplane (AUD)`) %>%
  rename(FuelConsumption = `Fuel Consumption, SAR (kg/km/seat)`,
         Emissions = `Fuel Consumption, EEA Master Emission Calculator (kg/km/seat)`,
         Cost = `Travel Cost by Airplane (AUD)`) %>%
  arrange(FuelConsumption, Emissions, Cost) %>%
  head(3)  # Select top 4 planes

# Select the top 4 most efficient cars based on fuel consumption and emissions
top_cars <- cars_data %>%
  filter(!is.na(`Comb (kg/km/seat)`) & !is.na(`CO2 Emissions(g/km)`) & !is.na(`Car Cost per Seat (AUD)`)) %>%
  select(Model, `Comb (kg/km/seat)`, `CO2 Emissions(g/km)`, `Car Cost per Seat (AUD)`) %>%
  rename(FuelConsumption = `Comb (kg/km/seat)`,
         Emissions = `CO2 Emissions(g/km)`,
         Cost = `Car Cost per Seat (AUD)`) %>%
  arrange(FuelConsumption, Emissions, Cost) %>%
  head(3)  # Select top 4 cars

# Combine planes and cars into a single dataset
combined_data <- rbind(
  data.frame(Vehicle = top_planes$`Aircraft Type`, Type = "Plane", 
             Metric = "FuelConsumption", Value = top_planes$FuelConsumption),
  data.frame(Vehicle = top_cars$Model, Type = "Car", 
             Metric = "FuelConsumption", Value = top_cars$FuelConsumption),
  data.frame(Vehicle = top_planes$`Aircraft Type`, Type = "Plane", 
             Metric = "Emissions", Value = top_planes$Emissions),
  data.frame(Vehicle = top_cars$Model, Type = "Car", 
             Metric = "Emissions", Value = top_cars$Emissions),
  data.frame(Vehicle = top_planes$`Aircraft Type`, Type = "Plane", 
             Metric = "Cost", Value = top_planes$Cost),
  data.frame(Vehicle = top_cars$Model, Type = "Car", 
             Metric = "Cost", Value = top_cars$Cost)
)

# Create the lollipop chart with `linewidth` instead of `size`
ggplot(combined_data, aes(x = Metric, y = Value, color = Type)) +
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = Value), 
               position = position_dodge(width = 0.5), linewidth = 1) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  facet_wrap(~ Vehicle, scales = "free_y") +  # Show each car/plane in a separate facet
  labs(title = "Top 3 Most Efficient Planes and Cars Comparison",
       x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
