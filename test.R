library(shiny)
library(dplyr)
library(leaflet)
library(tidyr)
library(readr)
library(stringr)
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

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Map of Carifesta archives"),
  
  # Display map on screen
  leafletOutput('locations'),
  
  # Add filter for festival edition
  # Add filter for festival edition
  checkboxGroupInput("festival_edition", "Filter by festival edition",
                     choices = festival_list,
                     selected = festival_list,
                     inline = TRUE
  )
)

# Define server logic
# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$festival_edition, {
    selected_editions <- str_split(input$festival_edition, ";")
    carchives_festival <- carchives[carchives$festival_edition %in% selected_editions, ]
    
    # Render map
    output$locations <- renderLeaflet({
      locations <- leaflet(data = carchives_festival)
      locations <- addTiles(locations)
      
      locations <- addMarkers(locations,
                              lng = ~Longitude, lat = ~Latitude,
                              popup = paste("Repository:", carchives_festival$repository,
                                            "<br>", "Label:",
                                            "<a href=", carchives_festival$repo_url, ">",
                                            carchives_festival$label, "</a>",
                                            "<br>", "Description:", carchives_festival$description,
                                            "<br>", "Files:", carchives_festival$folders),
                              clusterOptions = markerClusterOptions(
                                maxClusterSize = 100,
                                color = "red",
                                opacity = 0.5
                              )
      )
    })
  })
}

# Run application
shinyApp(ui = ui, server = server)


rsconnect::deployApp('/Users/renekooiker/Desktop/Digital Projects/carifesta-archives/carifesta-archives/')