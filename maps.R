library(ggplot2)
library(dplyr)
library(ggmap)
library(maps)
library(gridExtra)  # For combining the map and table

# Define the distance between Melbourne and Perth (in km)
distance_melbourne_perth <- 3400  # Distance in kilometers

# Top 4 efficient planes and cars as previously selected (using the same top_cars and top_planes)
# Add columns for total fuel consumption, emissions, and cost for the trip
top_planes <- top_planes %>%
  mutate(TotalFuelConsumption = FuelConsumption * distance_melbourne_perth,
         TotalEmissions = Emissions * distance_melbourne_perth,
         TotalCost = Cost * distance_melbourne_perth)

top_cars <- top_cars %>%
  mutate(TotalFuelConsumption = FuelConsumption * distance_melbourne_perth,
         TotalEmissions = Emissions * distance_melbourne_perth,
         TotalCost = Cost * distance_melbourne_perth)

# Get map data for Australia
aus_map <- map_data("world", region = "Australia")

# Define coordinates for Melbourne and Perth
melbourne_coords <- c(144.9631, -37.8136)  # (longitude, latitude)
perth_coords <- c(115.8575, -31.9505)

# Plot the map
map_plot <- ggplot() +
  geom_polygon(data = aus_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(aes(x = melbourne_coords[1], y = melbourne_coords[2]), color = "blue", size = 3) +  # Melbourne
  geom_point(aes(x = perth_coords[1], y = perth_coords[2]), color = "red", size = 3) +  # Perth
  geom_text(aes(x = melbourne_coords[1], y = melbourne_coords[2]), label = "Melbourne", hjust = -0.2, vjust = -1) +
  geom_text(aes(x = perth_coords[1], y = perth_coords[2]), label = "Perth", hjust = 1, vjust = -1) +
  geom_curve(aes(x = melbourne_coords[1], y = melbourne_coords[2], xend = perth_coords[1], yend = perth_coords[2]), 
             curvature = -0.2, color = "blue", arrow = arrow(type = "closed", length = unit(0.3, "cm"))) +
  labs(title = "Travel Route from Melbourne to Perth", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Create the comparison table data
comparison_table <- rbind(
  data.frame(Vehicle = top_planes$`Aircraft Type`, Type = "Plane", 
             TotalFuelConsumption = round(top_planes$TotalFuelConsumption, 2),
             TotalEmissions = round(top_planes$TotalEmissions, 2),
             TotalCost = round(top_planes$TotalCost, 2)),
  data.frame(Vehicle = top_cars$Model, Type = "Car", 
             TotalFuelConsumption = round(top_cars$TotalFuelConsumption, 2),
             TotalEmissions = round(top_cars$TotalEmissions, 2),
             TotalCost = round(top_cars$TotalCost, 2))
)

# Print the table for reference
print(comparison_table)

# Create a table plot
table_plot <- tableGrob(comparison_table)

# Combine the map and table
grid.arrange(map_plot, table_plot, ncol = 1, heights = c(2/3, 1/3))
