library(shiny)
library(dplyr)
library(leaflet)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(rsconnect)

# Set working directory
#setwd("~/Desktop/Digital Projects/carifesta-archives/carifesta-archives")

# Read CSV file
df <- read_csv("carifesta.csv")

# Separate coordinates column into Latitude and Longitude columns
df <- separate(df, col = coordinates, into = c("Latitude", "Longitude"), sep = ",")

# Convert coordinates columns to numeric values
df$Longitude <- as.numeric(df$Longitude)
df$Latitude <- as.numeric(df$Latitude)
df$access_status <- as.character(df$access_status)

festival_list <- c("Carifesta 1972" = "ca72",
                   "Carifesta 1976" = "ca76",
                   "Carifesta 1979" = "ca79",
                   "Carifesta 1981" = "ca81",
                   "Carifesta 1988" = "ca88",
                   "Carifesta 1992" = "ca92",
                   "Carifesta 1995" = "ca95",
                   "Carifesta 2000" = "ca00",
                   "Carifesta 2003" = "ca03",
                   "unknown" = "NULL")

df$festival_edition <- str_split(df$festival_edition, ";")

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
    df_festival <- df %>% filter(map_lgl(festival_edition, ~any(.x %in% input$festival)))
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
                                          "<br>", "Folders:", data()$folders,
                                          "<br>", "Access:", data()$access_status),
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

#rsconnect::deployApp(appDir = "/Users/renekooiker/Desktop/Digital Projects/carifesta-archives/carifesta-archives")
