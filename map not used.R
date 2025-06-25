# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)  # Make sure tidyr is loaded for pivot_longer

# Assumptions
lifetime_distance_car <- 200000  # in km
lifetime_distance_plane <- 50000000  # in km

# Seat capacity for the top 3 planes (adjust as necessary)
seat_capacity <- c("Airbus A220-300" = 150, "Airbus A320neo" = 240, "Airbus A330-900" = 460)

# Adjust top planes dataset with seat capacity and calculate lifetime emissions and fuel consumption
top_planes <- top_planes %>%
  mutate(NumberOfSeats = seat_capacity[`Aircraft Type`],  # Add seat capacity to the planes
         AdjustedEmissions = Emissions * NumberOfSeats,    # Adjust emissions by seat count
         LifetimeFuelConsumption = FuelConsumption * lifetime_distance_plane,  # Lifetime fuel consumption
         LifetimeEmissions = AdjustedEmissions * lifetime_distance_plane)  # Adjust lifetime emissions

# Calculate lifetime fuel consumption and emissions for top 3 cars
top_cars <- top_cars %>%
  mutate(LifetimeFuelConsumption = FuelConsumption * lifetime_distance_car,
         LifetimeEmissions = Emissions * lifetime_distance_car) %>%
  head(3)  # Select the top 3 cars

# Select the top 3 planes for comparison
top_planes <- top_planes %>%
  head(3)

# Combine planes and cars data into one dataset for visualization
combined_lifetime_data <- rbind(
  data.frame(Vehicle = top_planes$`Aircraft Type`, Type = "Plane", 
             LifetimeFuelConsumption = top_planes$LifetimeFuelConsumption,
             LifetimeEmissions = top_planes$LifetimeEmissions),
  data.frame(Vehicle = top_cars$Model, Type = "Car", 
             LifetimeFuelConsumption = top_cars$LifetimeFuelConsumption,
             LifetimeEmissions = top_cars$LifetimeEmissions)
)

# Reshape data to long format for ggplot
combined_long <- combined_lifetime_data %>%
  pivot_longer(cols = c(LifetimeFuelConsumption, LifetimeEmissions),
               names_to = "Metric", values_to = "Value")

# Plot grouped bar chart
ggplot(combined_long, aes(x = Vehicle, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Metric, scales = "free_y") +  # Split the plot by metric (Fuel vs Emissions)
  labs(title = "Lifetime Fuel Consumption and CO2 Emissions of Top 3 Cars and Planes",
       x = "Vehicle", y = "Total Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
