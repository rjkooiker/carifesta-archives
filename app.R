# Project to map the location of archives containing Carifesta materials worldwide.

# load packages
library(shiny)
library(dplyr)
library(leaflet)
library(tidyr)
library(readr)

# set working directory
# setwd("~/Desktop/Academic Work/R-Shiny/carifesta-archives")

# locally saved csv file
carchives <- read_csv("carifesta-archives.csv")

# remove commas in coordinates column
carchives$coordinates<-gsub(",", " ", carchives$coordinates)

# separating lat and long from coordinates column
carchives<-separate(carchives, col=coordinates, into=c("Latitude","Longitude"), sep=" ")

# change chr coordinates into num values
carchives$Longitude<-as.numeric(carchives$Longitude)
carchives$Latitude<-as.numeric(carchives$Latitude)

# define UI for application
ui <- fluidPage(

    # application title
    titlePanel("Map of Carifesta archives"),
    
    # display map on screen
    leafletOutput("locations")

    # add filter for festival_edition
  checkboxGroupInput("festival_edition", "Filter by festival edition",
                    choices = c("ca72", "ca76", "ca79", "ca81", "ca88", "ca92"),
                    inline = TRUE)


)
# define server logic ---
server <- function(input, output) {
  
  output$locations<-renderLeaflet({
    
    # set up an empty map
    
    locations<-leaflet(data=carchives)
    locations<-addTiles(locations)

    # Filter the data by festival edition
    if (any(input$festival_edition %in% carchives$category)) {
      locations <- locations %>%
        filter(category %in% input$festival_edition)
    }
    
    # put markers on the map
    
    locations <- addMarkers(
      # adding clustering options for overlapping points
      clusterOptions = markerClusterOptions(
        maxClusterSize = 100,
        color = "red",
        opacity = 0.5
      ),
      locations,
      lng = ~Longitude,
      lat = ~Latitude,
      # write popups in html
      popup = paste("Repository:", carchives$repository,
                    "<br>", "Label:",
                    '<a href=', carchives$repo_url, '>', carchives$label,
                    '</a>',
                    "<br>", "Description:", carchives$description,
                    "<br>", "Files:", carchives$folders
      ))
    
  })
  
}

# run the application 
shinyApp(ui = ui, server = server)

# deploy app
rsconnect::deployApp('/Users/renekooiker/Desktop/Digital Projects/carifesta-archives/carifesta-archives/')