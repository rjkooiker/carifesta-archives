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
                   "Carifesta 2017" = "ca17",
                   "unknown" = "NULL")

df$festival_edition <- str_split(df$festival_edition, ";")

# Define UI
ui <- fluidPage(
  titlePanel("Map of Carifesta Archives"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "festival",
                         label = "Filter by Festival Edition:",
                         choices = festival_list,
                         selected = festival_list,
                         inline = TRUE),
      actionButton("select_all", "Select All"),
      actionButton("deselect_all", "Deselect All"),
      tags$br(),tags$br(),
      selectInput("digital", "Filter by Online Availability:", 
                  choices = c("Online and On-Site" = "Online and On-Site", "Available Online" = TRUE, "Not Available Online" = FALSE), 
                  selected = "Online and On-Site"),
      selectInput("repo_type", "Repository Type:", 
                  choices = c("All" = "All", unique(df$repo_type)), 
                  selected = "All")
    ),
    
    mainPanel(
      leafletOutput("locations")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Add reactive code to respond to "Select All" and "Deselect All" buttons
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "festival", selected = festival_list)
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "festival", selected = character(0))
  })

  filtered_data <- reactive({
    if (input$digital == "Online and On-Site") {
      if (input$repo_type == "All") {
        df %>%
          filter(sapply(festival_edition, function(x) any(x %in% input$festival)))
      } else {
        df %>%
          filter(
            sapply(festival_edition, function(x) any(x %in% input$festival)),
            repo_type %in% input$repo_type
          )
      }
    } else {
      if (input$repo_type == "All") {
        df %>%
          filter(
            digital == as.logical(input$digital),
            sapply(festival_edition, function(x) any(x %in% input$festival))
          )
      } else {
        df %>%
          filter(
            digital == as.logical(input$digital),
            sapply(festival_edition, function(x) any(x %in% input$festival)),
            repo_type %in% input$repo_type
          )
      }
    }
  })
  
output$locations <- renderLeaflet({
    leaflet(filtered_data(), options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude, lat = ~Latitude,
        popup = paste("Repository:", filtered_data()$repository,
                      "<br>", "Label:",
                      paste0("<a href='", filtered_data()$repo_url, "' target='_blank'>", filtered_data()$label, "</a>"),
                      "<br>", "Description:", filtered_data()$description,
                      "<br>", "Folders:", filtered_data()$folders,
                      "<br>", "Online Availability:", ifelse(filtered_data()$digital, "Available Online", "Not Available Online")),
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
