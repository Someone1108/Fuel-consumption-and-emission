# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the datasets
planes_data <- read_csv("Final_plane_emmision_cost_efficiency.csv")
cars_data <- read_csv("FINAL_Updated_Normalized_Car_Fuel_Data_with_Car_Cost.csv")

# Prepare planes data with fuel consumption and emissions
top_planes <- planes_data %>%
  filter(!is.na(`Fuel Consumption, SAR (kg/km/seat)`) & !is.na(`Fuel Consumption, EEA Master Emission Calculator (kg/km/seat)`)) %>%
  select(`Aircraft Type`, `Fuel Consumption, SAR (kg/km/seat)`, `Fuel Consumption, EEA Master Emission Calculator (kg/km/seat)`) %>%
  rename(FuelConsumption = `Fuel Consumption, SAR (kg/km/seat)`,
         Emissions = `Fuel Consumption, EEA Master Emission Calculator (kg/km/seat)`) %>%
  mutate(Type = "Plane") %>%
  head(50)

# Prepare cars data with fuel consumption and emissions
top_cars <- cars_data %>%
  filter(!is.na(`Comb (kg/km/seat)`) & !is.na(`CO2 Emissions(g/km)`)) %>%
  select(Model, `Comb (kg/km/seat)`, `CO2 Emissions(g/km)`) %>%
  rename(FuelConsumption = `Comb (kg/km/seat)`,
         Emissions = `CO2 Emissions(g/km)`) %>%
  mutate(Type = "Car") %>%
  head(50)

# Combine both datasets
combined_data <- bind_rows(
  top_planes %>% rename(Name = `Aircraft Type`),
  top_cars %>% rename(Name = Model)
)

# Create the scatter plot without labels
ggplot(combined_data, aes(x = FuelConsumption, y = Emissions, color = Type)) +
  geom_point(size = 3) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Fuel Consumption vs Emissions for Planes and Cars",
       x = "Fuel Consumption (kg/km/seat)",
       y = "Emissions (g/km/seat)",
       color = "Vehicle Type") +
  theme_minimal()

