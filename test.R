library(shiny)
library(dplyr)
library(leaflet)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
# Set working directory
setwd("~/Desktop/Digital Projects/carifesta-archives/carifesta-archives")


# Read CSV file
carchives <- read_csv("carifesta-archives.csv")

# Separate coordinates column into Latitude and Longitude columns
carchives <- separate(carchives, col = coordinates, into = c("Latitude", "Longitude"), sep = ",")

# Convert coordinates columns to numeric values
carchives$Longitude <- as.numeric(carchives$Longitude)
carchives$Latitude <- as.numeric(carchives$Latitude)

festival_list <- c("carifesta_1972" = "ca72",
                   "carifesta_1976" = "ca76",
                   "carifesta_1979" = "ca79",
                   "carifesta_1981" = "ca81",
                   "carifesta_1988" = "ca88",
                   "carifesta_1992" = "ca92",
                   "carifesta_1995" = "ca95",
                   "carifesta_2000" = "ca00",
                   "unknown" = "NULL")

carchives$festival_edition <- str_split(carchives$festival_edition, ";")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Map of Carifesta archives"),
  
  # Display map on screen
  leafletOutput("locations"),
  
  # Add filter for festival edition
  
  checkboxGroupInput(inputId = "festival",
                     label = "Filter by festival",
                     choices = festival_list,
                     selected = festival_list,
                     inline = TRUE
  )
)

# Define server logic

server <- function(input, output, session) {
  
  
  data <- reactive({
    req(input$festival)
    carchives_festival <- carchives %>% filter(map_lgl(festival_edition, ~any(.x %in% input$festival)))
  })
  
  # Render map
  output$locations <- renderLeaflet({
    locations <- leaflet(data())
    locations <- addTiles(locations)
    locations <- addMarkers(locations,
                            lng = ~Longitude, lat = ~Latitude,
                            popup = paste("Repository:", data()$repository,
                                          "<br>", "Label:",
                                          "<a href=", data()$repo_url, ">",
                                          data()$label, "</a>",
                                          "<br>", "Description:", data()$description,
                                          "<br>", "Files:", data()$folders),
                            clusterOptions = markerClusterOptions(
                              showCoverageOnHover = FALSE,
                              maxClusterSize = 100,
                              opacity = 0.5
                            )
    )
  })
}
# Run application
shinyApp(ui = ui, server = server)


rsconnect::deployApp('/Users/renekooiker/Desktop/Digital Projects/carifesta-archives/carifesta-archives/')
