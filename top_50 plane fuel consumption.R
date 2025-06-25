ggplot(planes_data, aes(x = reorder(`Aircraft Type`, `Fuel Consumption, SAR (kg/km/seat)`), y = `Fuel Consumption, SAR (kg/km/seat)`)) +
  geom_point(size = 2, color = "#F08080") +  # Replacing "red" with the coral hex code
  labs(title = "Fuel Consumption by Aircraft Type",
       x = "Aircraft Type",
       y = "Fuel Consumption (kg/km)") +
  theme_minimal() +
  coord_flip()
