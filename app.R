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

    # add filter for festival_edition - make sure there is a selection by default
    checkboxGroupInput("festival_edition", "Filter by festival edition",
                    choices = c("Carifesta 72" = "ca72", 
                                "Carifesta 76" = "ca76", 
                                "Carifesta 79" = "ca79",
                                "Carifesta 81" = "ca81",
                                "Carifesta 88" = "ca88", 
                                "Carifesta 92" = "ca92"),
                    selected = c("ca72", "ca76", "ca79", "ca81", "ca88", "ca92"),
                    inline = TRUE
                    )
)
# define server logic ---
server <- function(input, output) {
  
  output$locations<-renderLeaflet({
    
    # set up an empty map
    
    locations<-leaflet(data=carchives)
    locations<-addTiles(locations)

    # filter the data by festival edition and make it reactive

    carchives_festival <- reactive(carchives[carchives$festival_edition %in% input$checkGroup, ])

    observeEvent(input$checkGroup, {
    leafletProxy("map", data = carchives_festval()) %>%
      clearGroup ("mygroup") %>% addMarkers(
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