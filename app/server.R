library(shiny)
library(choroplethr)
library(dplyr)
library(leaflet)
library(maps)
library(rgdal)
library(dplyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # data preparation 
  
  # From https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data
  NYCzipcodes <- readOGR("../data/ZIP_CODE_040114.shp",
                         #layer = "ZIP_CODE", 
                         verbose = FALSE)
  df <- read.csv('../data/sqs_pk_fy18_final.csv')
  
  selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(df$Zipcode))
  
  
  # ----- Transform to EPSG 4326 - WGS84 (required)
  subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
  
  # ----- save the data slot
  subdat_data=subdat@data[,c("ZIPCODE", "POPULATION")]
  subdat.rownames=rownames(subdat_data)
  #  subdat_data=
  #    subdat_data%>%left_join(count.df, by=c("ZIPCODE" = "region"))
  rownames(subdat_data)=subdat.rownames
  
  # ----- to write to geojson we need a SpatialPolygonsDataFrame
  subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
  
  # ----- set uo color pallette https://rstudio.github.io/leaflet/colors.html
  # Create a continuous palette function
  pal <- colorNumeric(
    palette = "Blues",
    domain = subdat$POPULATION
  )
  
  
    # simple map
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(-73.983,40.7639,zoom = 12) %>% 
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal(POPULATION),
                     label = ~paste0(ZIPCODE," Population: ",POPULATION),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "red",
                                                  bringToFront = TRUE))
    
  })
    
    #pre_k_map
    output$pre_k_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(-73.983,40.7639,zoom = 12) %>% 
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal(POPULATION),
                     label = ~paste0(ZIPCODE," Population: ",POPULATION),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "red",
                                                  bringToFront = TRUE))
      
    })
    
    output$el_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(-73.983,40.7639,zoom = 12) %>% 
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal(POPULATION),
                     label = ~paste0(ZIPCODE," Population: ",POPULATION),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "red",
                                                  bringToFront = TRUE))
      
    })
    
    output$hs_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(-73.983,40.7639,zoom = 12) %>% 
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal(POPULATION),
                     label = ~paste0(ZIPCODE,"Population: ",POPULATION),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "red",
                                                  bringToFront = TRUE))
      
    })
})

