# Loading required libraries
library(ggplot2)
library(shiny)
library(leaflet)

options(warn=-1) #turn off warnings
# Loading data in R
data <- read.csv("coral-bleaching-data.csv", header = TRUE)

data$coralType <- as.factor(data$coralType)
# % Bleaching is read as factor with percentage sign. Drop sign and convert to numeric
data$value <- sapply(data$value, function(x) gsub("%", "", x))
data$value <- as.numeric(data$value)

# Re-order levels of sites based on latitudes in decreasing order
order = levels(reorder(data$location, data$latitude))
data$location <-factor(data$location, levels = order)
data$location <- factor(data$location, levels=rev(levels(data$location)))

# Pulling out latitudes and longitudes of sites in a separate df for the map
sites_geoData = unique(data[c("location", "latitude", "longitude")])

# Shiny Portion of the Code
# UI
ui <- fluidPage(
  sidebarLayout(
    # Inputs to the App
    sidebarPanel(
      # Select the Coral Type
      selectInput(inputId = "selected_coral",
                         label = "Select the type of coral:",
                         choices = levels(data$coralType),
                         selected = c("blue corals")),
      # Select the smoother
      selectInput(inputId = "selected_smoother",
                         label = "Select the smoother:",
                         choices = c("lm","loess","gam"),
                         selected = c("lm"))
    ),
    # Outputs
    mainPanel(
      plotOutput(outputId = "bleaching_plot"),
      
      leafletOutput("sites_map")
    )
  )
)
# Server
server <- function(input, output, session) {
  # bleaching plot
  output$bleaching_plot <- renderPlot(
    ggplot(subset(data,coralType == input$selected_coral), aes(x = year, y = value)) + 
      geom_point(color = "blue", size = 1) +
      facet_grid(location ~ coralType) +
      geom_smooth(method = input$selected_smoother)
  )
  
  # leaflet map
  output$sites_map <- renderLeaflet({ 
    leaflet(data = sites_geoData) %>% 
      addTiles() %>%
      addMarkers(~longitude, ~latitude, label = ~as.character(location),
                 labelOptions = labelOptions(noHide = F) )
  })

}

shinyApp(ui, server)