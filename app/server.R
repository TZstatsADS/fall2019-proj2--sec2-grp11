library(shiny)
library(choroplethr)
library(dplyr)
library(leaflet)
library(maps)
library(rgdal)
library(dplyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

    output$map <- renderLeaflet({
      
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(-73.983,40.7639,zoom = 12) 
    


  })
    
})