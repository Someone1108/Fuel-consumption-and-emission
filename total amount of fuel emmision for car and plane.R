# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)  # For formatting the y-axis numbers

# Global assumptions
total_planes_world <- 25000  # Total number of planes in the world
total_cars_world <- 1270000000  # Total number of cars in the world
avg_seat_capacity_car <- 5  # Average number of passengers for cars
lifetime_distance_car <- 200000  # in km
lifetime_distance_plane <- 50000000  # in km
kg_to_liters <- 1.25  # Conversion factor for fuel (kg to liters)
grams_to_tons <- 1e-6  # Conversion factor for CO2 emissions (grams to metric tons)

# Seat capacity for the most efficient plane (adjust as necessary)
seat_capacity <- c("Airbus A220-300" = 150, "Airbus A320neo" = 240, "Airbus A330-900" = 460)

# Select the most efficient plane (assuming it's the one at the top of top_planes)
most_efficient_plane <- top_planes %>%
  head(1) %>%
  mutate(NumberOfSeats = seat_capacity[`Aircraft Type`],  # Add seat capacity
         AdjustedEmissions = Emissions * NumberOfSeats,    # Adjust emissions by seat count
         LifetimeFuelConsumption = FuelConsumption * lifetime_distance_plane * kg_to_liters,  # Convert kg to liters
         LifetimeEmissions = AdjustedEmissions * lifetime_distance_plane * grams_to_tons,  # Convert grams to metric tons
         LifetimeCost = Cost * lifetime_distance_plane)  # Lifetime cost in AUD

# Calculate total values for all planes globally
total_plane_impact <- most_efficient_plane %>%
  mutate(TotalFuelConsumption = LifetimeFuelConsumption * total_planes_world,  # Total fuel in liters
         TotalEmissions = LifetimeEmissions * total_planes_world,  # Total CO2 in metric tons
         TotalCost = LifetimeCost * total_planes_world)  # Total cost in AUD

# Select the most efficient car (assuming it's the one at the top of top_cars)
most_efficient_car <- top_cars %>%
  head(1) %>%
  mutate(AdjustedEmissions = Emissions / avg_seat_capacity_car,  # Adjust emissions by 5 seats
         AdjustedCost = Cost / avg_seat_capacity_car,  # Adjust cost by 5 seats
         LifetimeFuelConsumption = FuelConsumption * lifetime_distance_car * kg_to_liters,  # Convert kg to liters
         LifetimeEmissions = AdjustedEmissions * lifetime_distance_car * grams_to_tons,  # Convert grams to metric tons
         LifetimeCost = AdjustedCost * lifetime_distance_car)  # Lifetime cost in AUD

# Calculate total values for all cars globally
total_car_impact <- most_efficient_car %>%
  mutate(TotalFuelConsumption = LifetimeFuelConsumption * total_cars_world,  # Total fuel in liters
         TotalEmissions = LifetimeEmissions * total_cars_world,  # Total CO2 in metric tons
         TotalCost = LifetimeCost * total_cars_world)  # Total cost in AUD

# Combine total global impact for cars and planes into a single dataset
combined_global_impact <- rbind(
  data.frame(Vehicle = most_efficient_plane$`Aircraft Type`, Type = "Plane", 
             TotalFuelConsumption = total_plane_impact$TotalFuelConsumption,
             TotalEmissions = total_plane_impact$TotalEmissions,
             TotalCost = total_plane_impact$TotalCost),
  data.frame(Vehicle = most_efficient_car$Model, Type = "Car", 
             TotalFuelConsumption = total_car_impact$TotalFuelConsumption,
             TotalEmissions = total_car_impact$TotalEmissions,
             TotalCost = total_car_impact$TotalCost)
)

# Reshape data to long format for ggplot
combined_long_global <- combined_global_impact %>%
  pivot_longer(cols = c(TotalFuelConsumption, TotalEmissions, TotalCost),
               names_to = "Metric", values_to = "Value")

# Plot grouped bar chart for total global impact with unit conversions
ggplot(combined_long_global, aes(x = Vehicle, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Metric, scales = "free_y") +  # Split the plot by metric (Fuel, Emissions, Cost)
  labs(title = "Global Fuel Consumption, CO2 Emissions, and Cost for All Cars and Planes",
       x = "Vehicle", y = "Total Value") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +  # Format y-axis using short scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
