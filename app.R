#Aman Roy
#35011203

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyjs)
library(leaflet)  
library(geosphere)
library(shinythemes)

# Load the datasets
car_data <- read.csv("Car_dataset.csv")
plane_data <- read.csv("Plane_dataset.csv")

#lifetime mileage for cars and planes
car_lifetime_mileage <- 320000  # 200k miles to km
plane_lifetime_mileage <- 84490350 # 52 million miles to km 

# Global car and plane numbers 
total_cars <- 1400000000  # 1.4 billion cars
total_planes <- 39000 # number of active planes   

# city locations
locations <- data.frame(
  city = c("Melbourne", "Sydney", "Perth", "Brisbane", "Adelaide", "Canberra", "Darwin"),
  lat = c(-37.8136, -33.8688, -31.9505, -27.4698, -34.9285, -35.2809, -12.4634),
  lon = c(144.9631, 151.2093, 115.8605, 153.0251, 138.6007, 149.1300, 130.8456)
)

# Function to calculate distance between two cities
calculate_distance <- function(source, destination) {
  source_coords <- locations %>% filter(city == source) %>% select(lon, lat)
  dest_coords <- locations %>% filter(city == destination) %>% select(lon, lat)
  
  dist <- distHaversine(as.matrix(source_coords), as.matrix(dest_coords)) / 1000  # Convert to km
  return(dist)
}

ui <- fluidPage(
  useShinyjs(), 
  theme = shinytheme("cosmo"),
  
 
  tags$head(
    tags$style(HTML("
    /* Set a dark background for the entire page */
    body {
      background-color: #1b1b1b; /* Dark background */
      color: #00FFAB; 
      font-family: Arial, sans-serif;
    }

    /* Center content within div */
    .center-content {
      display: flex;
      flex-direction: column;
      align-items: center;
      max-width: 1200px;
      margin: 0 auto;
      color: #00FFAB; /* Neon green */
    }
    
  
    h1, h2, h3, h4, h5, h6 {
      color: #00FFAB;
      font-weight: bold;
      text-shadow: 0 0 1x #00FFAB, 0 0 1px #00FFAB;
    }
    
    /* Style paragraphs */
    p {
      color: #A9A9A9; 
      text
    }

    
    .shiny-input-container, .shiny-input-radiogroup label {
      color: #00FFAB;
    }
    .btn {
      background-color: #00FFAB; 
      color: #1b1b1b;
      border: none;
      padding: 10px 20px;
      font-weight: bold;
      transition: background-color 0.3s ease;
    }
    .btn:hover {
      background-color: #FFFFFF;
      color: #1b1b1b;
    }

    /* Radio buttons styling */
    .shiny-input-radiogroup .radio label {
      background-color: #333333; 
      padding: 8px 12px;
      border-radius: 5px;
      color: #00FFAB;
      display: inline-block;
      margin-bottom: 5px;
    }
    .shiny-input-radiogroup input:checked + label {
      background-color: #00FFAB;
      color: #1b1b1b;
    }

    /* Map container and chart container styling */
    .map-container, .first-chart-container {
      background-color: #333333; 
      border: 2px solid #00FFAB;
      border-radius: 10px;
      padding: 20px;
      margin-bottom: 20px;
    }
    
    /* Slider customization */
    .irs-bar,
    .irs-bar-edge,
    .irs-slider {
      background-color: #00FFAB;
      border-color: #00FFAB;
    }
    
    .irs-single {
      background-color: #00FFAB;
      color: #1b1b1b;
    }

    /* Tooltip styling for plotly */
    .plotly .modebar-group {
      background-color: #333333;
      color: #00FFAB;
    }
    .plotly .modebar-btn:hover {
      background-color: #00FFAB;
      color: #1b1b1b;
    }
  "))
  ),
  
  # Wrapper div to center all UI content
  div(class = "center-content",
      
      titlePanel("Fuel Consumption Comparison: Cars vs. Planes"),
      
      # Introduction Section
      fluidRow(
        column(width = 12,
               h4("Introduction"),
               p("In recent years there has been a growing trend of car manufacturers stopping the production of internal combustion engine vehicles in order to manage the fuel
             crises and reduce carbon emmisions, while this is the correct path there are many other modes of transports like planes which consume 
             large amounts of fuel in a single flight that a car could not even in an entire year and give out more co2 emmision aswell. This is the reason why i want to 
             compare cars and planes on different parameters to assess the difference between them."),
               p("The first question that may come to your mind is how is it possible to compare a plane against a car they both are totally different vehicles and have completly different
             uses, but to fix this issue all the data for the planes and cars are standardized so that the metrics can be compared per seat, which basically means that we compare the fuel
             consumption, emmision and cost for each seat in a plane and car per kilometer, this way both vheicles can be compared equally. ")
        )
      ),
      
      # Stacked Bar Chart Section
      fluidRow(
        column(
          width = 12,
          h4("Fuel Consumption by Model"),
          p("This stacked bar chart displays fuel consumption data for different models of cars and planes. You can search, sort the data and filter by model to focus on specific entries."),
          div(
            class = "map-container first-chart-container",  # Unique class here
            plotlyOutput("comparisonPlot", height = "600px", width = "100%")  # Adjust height if needed
          )
        )
      ),
      
      # Search and Filter Controls
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; justify-content: center; align-items: center; gap: 10px; margin-bottom: 10px;",
            textInput("search", "Search for Model or Make:", ""),
            actionButton("clear_search", "Clear Search", style = "background-color: #000000; color: white;")
          )
        )
      ),
      
      # Controls for Vehicle Type, Metric, Sort Order, and Number of Models per Page
      fluidRow(
        column(width = 3, 
               radioButtons("vehicle_type", "Select Vehicle Type:",
                            choices = c("Cars", "Planes"), selected = "Cars", inline = TRUE)
        ),
        column(width = 3,
               radioButtons("metric", "Select Metric to Display:",
                            choices = c("Fuel Consumption", "CO2 Emissions", "Cost per Seat"),
                            selected = "Fuel Consumption", inline = TRUE)
        ),
        column(width = 3,
               radioButtons("sort_order", "Select Sort Order:",
                            choices = c("Ascending", "Descending"), selected = "Descending", inline = TRUE)
        ),
        column(width = 3,
               sliderInput("num_per_page", "Number of Models per Page:", min = 10, max = 51, value = 51, step = 5)
        )
      ),
      
      # Page number Controls
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; justify-content: center; gap: 10px; align-items: center; margin-top: 10px;",
            actionButton("prev_page", "Previous", style = "background-color: #888888; color: white;"),
            textOutput("page_info", inline = TRUE),
            actionButton("next_page", "Next", style = "background-color: #000000; color: white;")
          )
        )
      ),
      
      # Map Visualization and Comparison of Car and Plane
      fluidRow(
        column(width = 12,
               h4("Route Comparison: Car vs Plane"),
               p("This map visualization compares the fuel consumption and cost for the selected car and plane model you can search for a particular car or plane for  
              a specific route. You can adjust the source and destination cities to see how these factors 
              change over different distances."),
               div(
                 class = "map-container",
                 leafletOutput("routeMap")
               )
               
        )
      ),
      
      # Route Input Controls
      fluidRow(
        column(width = 6,
               selectInput("source", "Source City", choices = locations$city, selected = "Perth"),
               selectInput("destination", "Destination City", choices = locations$city, selected = "Sydney")
        ),
        column(width = 6,
               selectizeInput("car_choice", "Search and Select Car:", choices = car_data$Model, multiple = FALSE),
               selectizeInput("plane_choice", "Search and Select Plane:", choices = plane_data$Aircraft.Type, multiple = FALSE)
        )
      ),
      
      # Comparison Charts
      fluidRow(
        column(width = 12,
               h4("Fuel Consumption Comparison"),
               p("This line chart compares fuel consumption between the selected car and plane on the chosen route, you can hover over the point to see the exact value."),
               div(
                 class = "map-container",
                 plotlyOutput("fuelComparisonPlot")
               )
        )
      ),
      
      fluidRow(
        column(width = 12,
               h4("Cost Comparison"),
               p("This line chart shows the estimated travel cost per seat for the selected car and plane, you can hover over the point to see the exact value."),
               div(
                 class = "map-container",
                 plotlyOutput("costComparisonPlot")
               )
        )
      ),
      
      fluidRow(
        column(width = 12,
               h4("CO2 Emissions Comparison"),
               p("This line chart illustrates CO2 emissions between the selected car and plane on the chosen route, you can hover over the point to see the exact value."),
               div(
                 class = "map-container",
                 plotlyOutput("co2ComparisonPlot")
               )
        )
      ),
      
      # Clustering Section
      fluidRow(
        column(width = 12,
               h4("Car Clustering Analysis"),
               p("This scatter plot visualizes the clustering of car models based on fuel consumption, CO2 emissions, 
              and cost per seat. You can select different features for clustering to explore various insights."),
               div(
                 class = "map-container",
                 plotlyOutput("clusterPlot")
               )
        )  # Clustering plot
      ),
      fluidRow(
        column(width = 6,
               sliderInput("clusters", "Select Number of Clusters:", min = 2, max = 6, value = 3, step = 1)
        ),
        column(width = 6,
               checkboxGroupInput("features", "Select Features for Clustering:", 
                                  choices = c("Fuel Consumption", "CO2 Emissions", "Cost per Seat"),
                                  selected = c("Fuel Consumption", "CO2 Emissions"))
        )
      ),
      fluidRow(
        column(width = 12,
               h4("Comparison for a single car against a single plane"),
               p("Judging by the graphs above the figures might suggest that traveling by air is the better option—it appears more cost-effective, produces fewer emissions, 
               and uses less fuel than cars. However, this interpretation is misleading due to the unit of measurement used in these figures: kg/km/seat, which represents 
               values per person for both planes and cars. It's important to consider that an average plane typically carries around 150 passengers, while a car carries about 5. 
               Additionally, a car travels approximately 200,000 miles over its lifetime, whereas a plane averages around 52 million miles. With this in mind, let’s compare the total 
               distance an average plane and car travel over their lifetimes.
               This bar chart compares the total CO2 emissions across selected car and plane models over their 
              entire lifetime mileage. looking at the graphs you can clearly see that the cost, fuel consumption and co2 emmision for palne is much higher than that of any car
             over a set distance."),
               div(
                 class = "map-container",
                 plotlyOutput("singleMetricPlot")
               )
        )
      ),
      fluidRow(
        column(width = 12,
               radioButtons("metric_choice", "Select Metric to Display:",
                            choices = c("Fuel Consumption", "Cost", "CO2 Emissions"),
                            selected = "Fuel Consumption", inline = TRUE)
        )
      ),
      fluidRow(
        column(width = 12,
               h4("Global Comparison: Total Number of Cars vs Planes"),
               p("when you consider the total amount of cars and planes which is number of planes and cars in the world is astronomically high, 
                 there are around 25000 active planes in the world and around 1.27 billion internal combustion engines in the world and when we take that in account the values shift 
                 too much, the difference is so high that I had to use a logarithmic scale to show them in the same graph"),
               div(
                 class = "map-container",
                 plotlyOutput("globalPerformanceComparison")
               )
        )
      ),
      fluidRow(
        column(width = 12,
               radioButtons("global_metric_choice", "Select Global Metric to Display:",
                            choices = c("Fuel Consumption", "CO2 Emissions"),
                            selected = "Fuel Consumption", inline = TRUE)
        )
      ),
      
      # Conclusion Section
      fluidRow(
        column(width = 12,
               h4("Conclusion"),
               p("To answer the question that what is better for a single person cars are a better option through out but because of the fact that planes
             travel shuch long distances that it increases the carbon footprint per person but when we take a global view we can see that the sheer amount
             of cars that are operating in this world significantly out wieghs the carbon footprint produced by planes.")
        )
      ),
      
      # About Data Section
      fluidRow(
        column(width = 12,
               h4("About Data"),
               p("For this project, I used multiple datasets for planes and cars. These datasets were taken from multiple sources named below:"),
               
               p(tags$strong("1. 2022 Fuel Consumption Ratings"),
                 br(),
                 "This dataset shows tabular data in CSV format with ~1K rows x 15 columns. It provides model-specific fuel consumption ratings and estimated carbon dioxide emissions for new light-duty vehicles for retail sale in Canada in 2022.",
                 br(),
                 "URL: ", tags$a(href="https://www.kaggle.com/datasets/rinichristy/2022-fuel-consumption-ratings", "2022 Fuel Consumption Ratings"),
                 
               ),
               
               p(tags$strong("2. Petrol/Gas Prices Worldwide"),
                 br(),
                 "This dataset shows fuel prices worldwide for 2022, containing columns like price per litre for different countries. It is intended for comparative analysis.",
                 br(),
                 "URL: ", tags$a(href="https://www.kaggle.com/datasets/zusmani/petrolgas-prices-worldwide", "Petrol/Gas Prices Worldwide"),
                 
               ),
               
               p(tags$strong("3. Australian Vehicle Prices"),
                 br(),
                 "This dataset contains information about cars available in Australia in 2023, including details on fuel type, distance in kilometers, fuel consumption, and price.",
                 br(),
                 "URL: ", tags$a(href="https://www.kaggle.com/datasets/nelgiriyewithana/australian-vehicle-prices/data", "Australian Vehicle Prices"),
                 
               ),
               
               p(tags$strong("4. Domestic Airlines - Top Routes and Totals"),
                 br(),
                 "This dataset provides information on national flight paths within Australia via RPT air service, including city pairs, distances, revenue passenger kilometers, and available seat kilometers.",
                 br(),
                 "URL: ", tags$a(href="https://www.data.gov.au/data/dataset/domestic-airlines-top-routes-and-totals", "Domestic Airlines - Top Routes and Totals"),
                 
               ),
               
               p(tags$strong("5. Airline Fuel Cost and Consumption"),
                 br(),
                 "This dataset covers major airlines' monthly fuel consumption rates, international costs, and per-gallon fuel costs.",
                 br(),
                 "URL: ", tags$a(href="https://www.transtats.bts.gov/fuel.asp?20=E", "Airline Fuel Cost and Consumption"),
                 
               ),
               
               # Add similar paragraphs for datasets 6 and 7, using the same structure
               
               p(tags$strong("6. Fuel Consumption of the 50 Most Used Passenger Planes"),
                 br(),
                 "This dataset shows fuel consumption and CO2 emissions for 50 commonly used passenger planes, including various analysis sheets and metrics.",
                 br(),
                 "URL: ", tags$a(href="https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/4CYNKA", "Fuel Consumption of Passenger Planes"),
                 
               ),
               
               p(tags$strong("7. Airplanes Avfare 2024 Q1"),
                 br(),
                 "This dataset provides average ticket prices for American airports in Q1 of 2024.",
                 br(),
                 "URL: ", tags$a(href="https://www.transtats.bts.gov/AverageFare/", "Airplanes Avfare 2024 Q1"),
                 
               )
        )
      )
  )  # End of div class = "center-content"
)


server <- function(input, output, session) {
  
  # Define reactive values for pagination
  pagination <- reactiveValues(page = 1)
  
  # Dynamically update the max value of the slider based on vehicle type
  observe({
    max_models <- if (input$vehicle_type == "Cars") nrow(car_data) else nrow(plane_data)
    updateSliderInput(session, "num_per_page", max = min(51, max_models), value = min(51, max_models))
  })
  
  # data filtering for stacked bar chart
  selected_data <- reactive({
    data <- if (input$vehicle_type == "Cars") {
      car_data %>%
        select(Make, Model, Comb..kg.km.seat., CO2.Emissions.g.km., Car.Cost.per.Seat..AUD.) %>%
        rename(
          Fuel_Consumption = Comb..kg.km.seat.,
          CO2_Emissions = CO2.Emissions.g.km.,
          Cost_per_Seat = Car.Cost.per.Seat..AUD.
        )
    } else {
      plane_data %>%
        select(Aircraft.Type, Fuel.Consumption, 
               Fuel.Consumption..EEA.Master.Emission.Calculator..kg.km.seat., Travel.Cost.by.Airplane..AUD.) %>%
        rename(
          Model = Aircraft.Type,
          Fuel_Consumption = Fuel.Consumption,
          CO2_Emissions = Fuel.Consumption..EEA.Master.Emission.Calculator..kg.km.seat.,
          Cost_per_Seat = Travel.Cost.by.Airplane..AUD.
        )
    }
    
    # making sure that the Model column is unique
    data$Model <- make.unique(as.character(data$Model))
    
   
    if (input$vehicle_type == "Cars" && input$search != "") {
      data <- data %>%
        filter(grepl(input$search, Model, ignore.case = TRUE) | grepl(input$search, Make, ignore.case = TRUE))
    } else if (input$search != "") {
      data <- data %>% filter(grepl(input$search, Model, ignore.case = TRUE))
    }
    
  
    metric <- switch(input$metric,
                     "Fuel Consumption" = "Fuel_Consumption",
                     "CO2 Emissions" = "CO2_Emissions",
                     "Cost per Seat" = "Cost_per_Seat")
    
   
    if (input$sort_order == "Descending") {
      data <- data %>% arrange(desc(!!sym(metric)))
    } else {
      data <- data %>% arrange(!!sym(metric))  # Sort in ascending order
    }
    
    # Reorder the factor levels of Model based on the sorted metric
    data$Model <- factor(data$Model, levels = data$Model)
    
    return(data)
  })
  
  
  observeEvent(input$clear_search, {
    updateTextInput(session, "search", value = "")
  })
  
  
  max_page <- reactive({
    ceiling(nrow(selected_data()) / input$num_per_page)
  })
  
  
  output$page_info <- renderText({
    paste("Page", pagination$page, "of", max_page())
  })
  
 
  paginated_data <- reactive({
    data <- selected_data()
    start_row <- (pagination$page - 1) * input$num_per_page + 1
    end_row <- min(pagination$page * input$num_per_page, nrow(data))
    data[start_row:end_row, ]
  })
  
  
  observe({
    toggleState("prev_page", pagination$page > 1)
    toggleState("next_page", pagination$page < max_page())
  })
  

  observeEvent(input$next_page, {
    if (pagination$page < max_page()) {
      pagination$page <- pagination$page + 1
    }
  })
  
  
  observeEvent(input$prev_page, {
    if (pagination$page > 1) {
      pagination$page <- pagination$page - 1
    }
  })
  
 
  output$comparisonPlot <- renderPlotly({
    metric <- switch(input$metric,
                     "Fuel Consumption" = "Fuel_Consumption",
                     "CO2 Emissions" = "CO2_Emissions",
                     "Cost per Seat" = "Cost_per_Seat")
    
    # Set the color based on vehicle type
    fill_color <- if (input$vehicle_type == "Cars") {
      "#00C0C0"  # Color for cars
    } else {
      "#FF6F61"  # Color for planes
    }
    
 
    p <- ggplot(paginated_data(), aes(x = Model, y = .data[[metric]])) +
      geom_bar(stat = "identity", fill = fill_color) +
      coord_flip() + 
      labs(x = "Model", y = input$metric) +
      theme(axis.text.y = element_text(size = 10, angle = 0, hjust = 1, margin = margin(r = 10)))  # Adjust text margin for spacing
    
   
    ggplotly(p)
  })
  
  

  route_distance <- reactive({
    calculate_distance(input$source, input$destination)
  })
  #map visualization
  output$routeMap <- renderLeaflet({
    source_coords <- locations %>% filter(city == input$source)
    dest_coords <- locations %>% filter(city == input$destination)
    
    
    plane_route_coords <- gcIntermediate(
      c(source_coords$lon, source_coords$lat), 
      c(dest_coords$lon, dest_coords$lat),
      n = 100,  # Increase the number of points for more curve
      addStartEnd = TRUE
    )
    
    # Car route: a straight line
    car_route_coords <- rbind(c(source_coords$lon, source_coords$lat),
                              c(dest_coords$lon, dest_coords$lat))
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(c(source_coords$lon, dest_coords$lon)),
              lat = mean(c(source_coords$lat, dest_coords$lat)), zoom = 4) %>%
      
      # source marker (green)
      addCircleMarkers(lng = source_coords$lon, lat = source_coords$lat, 
                       color = "blue", radius = 8, label = "Source") %>%
      
      # destination marker (red)
      addCircleMarkers(lng = dest_coords$lon, lat = dest_coords$lat, 
                       color = "green", radius = 8, label = "Destination") %>%
      
      # car route as a straight line (with hover effect showing car model)
      addPolylines(lng = car_route_coords[,1], lat = car_route_coords[,2], 
                   color = "#FF6F61", weight = 3, opacity = 0.7,
                   label = paste("Car Model:", input$car_choice),  # Hover label for car route
                   labelOptions = labelOptions(noHide = FALSE, direction = "auto")) %>%
      
      # plane route as a curved line (with hover effect showing plane model)
      addPolylines(lng = plane_route_coords[,1], lat = plane_route_coords[,2], 
                   color = "#00C0C0", weight = 3, dashArray = "5, 10", opacity = 0.7, 
                   label = paste("Plane Model:", input$plane_choice),  # Hover label for plane route
                   labelOptions = labelOptions(noHide = FALSE, direction = "auto")) %>%
      
      # legend to explain the colors
      addLegend("bottomright", colors = c("blue", "green", "#00C0C0", "#FF6F61"), 
                labels = c("Source", "Destination", "Plane Route", "Car Route"), 
                opacity = 1)
  })
  
  
  # Separate plot for Fuel Consumption
  output$fuelComparisonPlot <- renderPlotly({
    car_data_filtered <- car_data %>% filter(Model == input$car_choice) %>% head(1)
    plane_data_filtered <- plane_data %>% filter(Aircraft.Type == input$plane_choice) %>% head(1)
    
    car_fuel <- car_data_filtered$Comb..kg.km.seat. * route_distance()
    plane_fuel <- plane_data_filtered$Fuel.Consumption * route_distance()
    
    comparison_data <- data.frame(
      Vehicle = c("Car", "Plane"),
      Fuel_Consumption = c(car_fuel, plane_fuel)
    )
    
    p <- ggplot(comparison_data, aes(x = Vehicle, y = Fuel_Consumption)) +
      geom_line(aes(group = 1), color = "blue") +
      geom_point(size = 3, color = "blue") +
      labs(x = "Vehicle", y = "Fuel Consumption (kg/km/seat)", title = "Fuel Consumption Comparison")
    
    ggplotly(p)
  })
  
  # Separate plot for Cost per Seat
  output$costComparisonPlot <- renderPlotly({
    car_data_filtered <- car_data %>% filter(Model == input$car_choice) %>% head(1)
    plane_data_filtered <- plane_data %>% filter(Aircraft.Type == input$plane_choice) %>% head(1)
    
    car_cost <- car_data_filtered$Car.Cost.per.Seat..AUD. * route_distance()
    plane_cost <- plane_data_filtered$Travel.Cost.by.Airplane..AUD. * route_distance()
    
    comparison_data <- data.frame(
      Vehicle = c("Car", "Plane"),
      Cost_per_Seat = c(car_cost, plane_cost)
    )
    
    p <- ggplot(comparison_data, aes(x = Vehicle, y = Cost_per_Seat)) +
      geom_line(aes(group = 1), color = "green") +
      geom_point(size = 3, color = "green") +
      labs(x = "Vehicle", y = "Cost per Seat (AUD)", title = "Cost Comparison")
    
    ggplotly(p)
  })
  
  # Separate plot for CO2 Emissions
  output$co2ComparisonPlot <- renderPlotly({
    car_data_filtered <- car_data %>% filter(Model == input$car_choice) %>% head(1)
    plane_data_filtered <- plane_data %>% filter(Aircraft.Type == input$plane_choice) %>% head(1)
    
    car_co2 <- car_data_filtered$CO2.Emissions.g.km. * route_distance()
    plane_co2 <- plane_data_filtered$Fuel.Consumption..EEA.Master.Emission.Calculator..kg.km.seat. * route_distance()
    
    comparison_data <- data.frame(
      Vehicle = c("Car", "Plane"),
      CO2_Emissions = c(car_co2, plane_co2)
    )
    
    p <- ggplot(comparison_data, aes(x = Vehicle, y = CO2_Emissions)) +
      geom_line(aes(group = 1), color = "red") +
      geom_point(size = 3, color = "red") +
      labs(x = "Vehicle", y = "CO2 Emissions (g/km/seat)", title = "CO2 Emissions Comparison") +
      scale_y_continuous(labels = scales::comma)  
    
    ggplotly(p)
  })
  # Clustering Plot Logic (Ensure it is isolated from other plots)
  output$clusterPlot <- renderPlotly({
    # Check if the selected features are less than two
    if (length(input$features) < 2) {
      return(plotly_empty() %>%
               layout(title = list(text = "Please select at least 2 attributes for clustering.",
                                   font = list(size = 16))))
    }
    
    
    req(input$clusters)
    
    
    feature_mapping <- list(
      "Fuel Consumption" = "Comb..kg.km.seat.",  
      "CO2 Emissions" = "CO2.Emissions.g.km.",        
      "Cost per Seat" = "Car.Cost.per.Seat..AUD."       
    )
    
    
    selected_features <- unlist(feature_mapping[input$features])
    
    
    cluster_data <- car_data %>%
      select(all_of(selected_features))  
    
    # Perform k-means clustering with the selected number of clusters
    set.seed(123)
    kmeans_result <- kmeans(cluster_data, centers = input$clusters)
    
    # Add cluster information to the data
    car_data$Cluster <- as.factor(kmeans_result$cluster)
    
    # Visualize clustering results
    p <- ggplot(car_data, aes(x = .data[[selected_features[1]]], y = .data[[selected_features[2]]], color = Cluster)) +
      geom_point(size = 3) +
      labs(title = "Car Clustering Based on Selected Features",
           x = selected_features[1],
           y = selected_features[2]) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  #plot to show the lifetime info of a single car/plane
  output$singleMetricPlot <- renderPlotly({
    # Filter the selected car and plane from input
    car_data_filtered <- car_data %>% filter(Model == input$car_choice)
    plane_data_filtered <- plane_data %>% filter(Aircraft.Type == input$plane_choice)
    
    # Calculate lifetime metrics
    car_lifetime_fuel <- car_data_filtered$Comb..kg.km.seat. * car_lifetime_mileage
    plane_lifetime_fuel <- plane_data_filtered$Fuel.Consumption * plane_lifetime_mileage
    car_lifetime_cost <- car_data_filtered$Car.Cost.per.Seat..AUD. * car_lifetime_mileage
    plane_lifetime_cost <- plane_data_filtered$Travel.Cost.by.Airplane..AUD. * plane_lifetime_mileage
    car_lifetime_co2 <- (car_data_filtered$CO2.Emissions.g.km. / 1000) * car_lifetime_mileage  # Conversion from grams to kg
    plane_lifetime_co2 <- plane_data_filtered$Fuel.Consumption..EEA.Master.Emission.Calculator..kg.km.seat. * plane_lifetime_mileage
    
    # Based on selected metric, prepare data
    metric_data <- switch(input$metric_choice,
                          "Fuel Consumption" = data.frame(Vehicle = c("Car", "Plane"),
                                                          Lifetime_Value = c(car_lifetime_fuel, plane_lifetime_fuel)),
                          "Cost" = data.frame(Vehicle = c("Car", "Plane"),
                                              Lifetime_Value = c(car_lifetime_cost, plane_lifetime_cost)),
                          "CO2 Emissions" = data.frame(Vehicle = c("Car", "Plane"),
                                                       Lifetime_Value = c(car_lifetime_co2, plane_lifetime_co2))
    )
    
    
    p <- ggplot(metric_data, aes(x = Vehicle, y = Lifetime_Value, fill = Vehicle, 
                                 text = paste(input$metric_choice, ":", Lifetime_Value))) +
      geom_bar(stat = "identity", width = 0.2) +
      labs(title = paste(input$metric_choice, "Comparison"),
           x = "Vehicle", y = "Total Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)  # Use comma formatting for large numbers
    
    
    ggplotly(p, tooltip = "text")
  })
  output$globalComparisonText <- renderText({
    paste("There are approximately 1.4 billion cars and", 
          format(total_planes, big.mark = ","), "planes globally.")
  })
  
  
  #total nubmer of cars and planes
  output$globalPerformanceComparison <- renderPlotly({
    global_data <- data.frame(
      Vehicle = c("Cars", "Planes"),
      Avg_Fuel_Consumption = c(7.6 * total_cars, 12 * total_planes),  
      Avg_CO2_Emissions = c(120 * total_cars / 1000, 90 * total_planes)      
    )
    
    # Based on selected global metric
    global_metric_data <- switch(input$global_metric_choice,
                                 "Fuel Consumption" = data.frame(Vehicle = global_data$Vehicle,
                                                                 Lifetime_Value = global_data$Avg_Fuel_Consumption),
                                 "CO2 Emissions" = data.frame(Vehicle = global_data$Vehicle,
                                                              Lifetime_Value = global_data$Avg_CO2_Emissions)
    )
    

    global_metric_data$Formatted_Value <- scales::comma_format()(global_metric_data$Lifetime_Value)
    
   
    p <- ggplot(global_metric_data, aes(x = Vehicle, y = Lifetime_Value, fill = Vehicle, 
                                        text = paste(input$global_metric_choice, ":", Formatted_Value))) +
      geom_bar(stat = "identity", width = 0.2) +
      labs(title = paste("Global", input$global_metric_choice, "Comparison: Cars vs Planes"),
           x = "Vehicle", y = "Total Value (Log Scale)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(trans = 'log10', labels = scales::comma)  # Use log10 scale and comma formatting
    
 
    ggplotly(p, tooltip = "text")
  })
  
}

shinyApp(ui = ui, server = server)
