# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

cars_data <- read_csv("FINAL_Updated_Normalized_Car_Fuel_Data_with_Car_Cost.csv")
# Filter to get the top 50 cars by fuel consumption
top_cars <- cars_data %>%
  arrange(`Comb (kg/km/seat)`) %>%
  head(50)

# Create a dot plot for the top 50 cars
ggplot(top_cars, aes(x = reorder(Model, `Comb (kg/km/seat)`), y = `Comb (kg/km/seat)`)) +
  geom_point(size = 2, color = "#00C8C8") +  # Replacing "blue" with the new cyan hex code
  labs(title = "Fuel Consumption by 50 most efficient Car Models",
       x = "Car Model",
       y = "Fuel Consumption (kg/km)") +
  theme_minimal() +
  coord_flip()  # Flipping the axes for better readability

