# Load necessary libraries
library(ggplot2)
library(maps)
library(ggspatial)

# Base map of Australia
australia_map <- map_data("world", region = "Australia")

# Define coordinates for cities
long_sydney <- 151.2093
lat_sydney <- -33.8688
long_melbourne <- 144.9631
lat_melbourne <- -37.8136
long_brisbane <- 153.0281
lat_brisbane <- -27.4698
long_perth <- 115.8575
lat_perth <- -31.9505

# Define costs for car and plane (for example purposes)
cost_for_car <- 1 # This value represents the thickness of the car route
cost_for_plane <- 1 # This value represents the thickness of the plane route

# Create the plot
ggplot() +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
  
  # Car route (Sydney to Melbourne)
  geom_curve(aes(x = long_sydney, y = lat_sydney, xend = long_melbourne, yend = lat_melbourne),
             curvature = 0.2, color = "blue", size = cost_for_car, linetype = "solid") +  # Adjust size for cost
  
  # Plane route (Sydney to Melbourne)
  geom_curve(aes(x = long_sydney, y = lat_sydney, xend = long_melbourne, yend = lat_melbourne),
             curvature = -0.2, color = "red", size = cost_for_plane, linetype = "dashed") +  # Adjust size for cost
  
  # Car route (Melbourne to Brisbane)
  geom_curve(aes(x = long_melbourne, y = lat_melbourne, xend = long_brisbane, yend = lat_brisbane),
             curvature = 0.3, color = "blue", size = cost_for_car, linetype = "solid") +
  
  # Plane route (Melbourne to Brisbane)
  geom_curve(aes(x = long_melbourne, y = lat_melbourne, xend = long_brisbane, yend = lat_brisbane),
             curvature = -0.3, color = "red", size = cost_for_plane, linetype = "dashed") +
  
  # Car route (Melbourne to Perth)
  geom_curve(aes(x = long_melbourne, y = lat_melbourne, xend = long_perth, yend = lat_perth),
             curvature = 0.4, color = "blue", size = cost_for_car, linetype = "solid") +
  
  # Plane route (Melbourne to Perth)
  geom_curve(aes(x = long_melbourne, y = lat_melbourne, xend = long_perth, yend = lat_perth),
             curvature = -0.4, color = "red", size = cost_for_plane, linetype = "dashed") +
  
  # Add labels or annotations
  annotate("text", x = long_sydney, y = lat_sydney, label = "Car: 100 AUD, 50 kg CO2", vjust = 1.5) +
  annotate("text", x = long_melbourne, y = lat_melbourne, label = "Plane: 150 AUD, 30 kg CO2", vjust = 1.5) +
  annotate("text", x = long_brisbane, y = lat_brisbane, label = "Brisbane", vjust = 1.5) +
  annotate("text", x = long_perth, y = lat_perth, label = "Perth", vjust = 1.5) +
  
  # Legend and titles
  labs(title = "Cost and Emission Comparison for Travel Routes",
       subtitle = "Comparing Car vs Plane travel between major Australian cities") +
  theme_minimal()

