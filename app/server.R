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
  load("../output/processed_ele_zipcode.RData")
  load("../output/processed_mid_zipcode.RData")
  load("../output/processed_k_8_zipcode.RData")
  load("../output/processed_hs_zipcode.RData")
  load("../output/processed_ele_total.RData")
  load("../output/processed_mid_total.RData")
  load("../output/processed_k_8_total.RData")
  load("../output/processed_hs_total.RData")
  pk2018 <- read.csv("../output/2018pkdata.final.csv")
  pk2016<-read.csv("../output/2016pkdata.final.csv")
  pk2017<-read.csv("../output/2017pkdata.final.csv")
  
  
  # simple map
  output$num_schools_map <- renderLeaflet({
    
    ## pk-number
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(pk2018$Zipcode))
    # ----- Transform to EPSG 4326 - WGS84 (required)
    subdat.pk<-spTransform(selZip, CRS("+init=epsg:4326"))
    
    subdat_data.pk = subdat.pk@data[,c("ZIPCODE","POPULATION")]
    subdat.pk.rownames = rownames(subdat_data.pk)
    subdat_data.pk$ZIPCODE <- as.character(subdat_data.pk$ZIPCODE)
    pk2018$Zipcode  <- as.character(pk2018$Zipcode)
    subdat_data.pk =
      subdat_data.pk %>% left_join(pk2018, by=c("ZIPCODE" = "Zipcode"))
    rownames(subdat_data.pk)= subdat.pk.rownames
    
    # ----- to write to geojson we need a SpatialPolygonsDataFrame
    subdat.pk<-SpatialPolygonsDataFrame(subdat.pk, data=subdat_data.pk)
    
    
    ## ele-number
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(ele_zipcode$zipcode))
    # ----- Transform to EPSG 4326 - WGS84 (required)
    subdat.ele<-spTransform(selZip, CRS("+init=epsg:4326"))
    ele_number <- ele_zipcode %>%
      filter(yr == 2018)
    
    subdat_data.ele=subdat.ele@data[,c("ZIPCODE","POPULATION")]
    subdat.ele.rownames=rownames(subdat_data.ele)
    subdat_data.ele$ZIPCODE <- as.character(subdat_data.ele$ZIPCODE)
    ele_number$zipcode  <- as.character(ele_number$zipcode)
    subdat_data.ele=
      subdat_data.ele%>%left_join(ele_number, by=c("ZIPCODE" = "zipcode"))
    rownames(subdat_data.ele)= subdat.ele.rownames
    
    # ----- to write to geojson we need a SpatialPolygonsDataFrame
    subdat.ele<-SpatialPolygonsDataFrame(subdat.ele, data=subdat_data.ele)
    
    ## mid-number
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(mid_zipcode$zipcode))
    # ----- Transform to EPSG 4326 - WGS84 (required)
    subdat.mid<-spTransform(selZip, CRS("+init=epsg:4326"))
    mid_number <- mid_zipcode %>%
      filter(yr == 2018)
    
    subdat_data.mid=subdat.mid@data[,c("ZIPCODE","POPULATION")]
    subdat.mid.rownames=rownames(subdat_data.mid)
    subdat_data.mid$ZIPCODE <- as.character(subdat_data.mid$ZIPCODE)
    mid_number$zipcode  <- as.character(mid_number$zipcode)
    subdat_data.mid=
      subdat_data.mid%>%left_join(mid_number, by=c("ZIPCODE" = "zipcode"))
    rownames(subdat_data.mid)= subdat.mid.rownames
    
    # ----- to write to geojson we need a SpatialPolygonsDataFrame
    subdat.mid<-SpatialPolygonsDataFrame(subdat.mid, data=subdat_data.mid)
    
    ## k_8-number
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(k_8_zipcode$zipcode))
    # ----- Transform to EPSG 4326 - WGS84 (required)
    subdat.k_8<-spTransform(selZip, CRS("+init=epsg:4326"))
    k_8_number <- k_8_zipcode %>%
      filter(yr == 2018)
    
    subdat_data.k_8=subdat.k_8@data[,c("ZIPCODE","POPULATION")]
    subdat.k_8.rownames=rownames(subdat_data.k_8)
    subdat_data.k_8$ZIPCODE <- as.character(subdat_data.k_8$ZIPCODE)
    k_8_number$zipcode  <- as.character(k_8_number$zipcode)
    subdat_data.k_8=
      subdat_data.k_8%>%left_join(k_8_number, by=c("ZIPCODE" = "zipcode"))
    rownames(subdat_data.k_8)= subdat.k_8.rownames
    
    # ----- to write to geojson we need a SpatialPolygonsDataFrame
    subdat.k_8<-SpatialPolygonsDataFrame(subdat.k_8, data=subdat_data.k_8)
    
    
    ## hs-number
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(hs_zipcode$zipcode))
    # ----- Transform to EPSG 4326 - WGS84 (required)
    subdat.hs<-spTransform(selZip, CRS("+init=epsg:4326"))
    hs_number <- hs_zipcode %>%
      filter(yr == 2018)
    
    subdat_data.hs=subdat.hs@data[,c("ZIPCODE","POPULATION")]
    subdat.hs.rownames=rownames(subdat_data.hs)
    subdat_data.hs$ZIPCODE <- as.character(subdat_data.hs$ZIPCODE)
    hs_number$zipcode  <- as.character(hs_number$zipcode)
    subdat_data.hs=
      subdat_data.hs%>%left_join(hs_number, by=c("ZIPCODE" = "zipcode"))
    rownames(subdat_data.hs)= subdat.hs.rownames
    
    # ----- to write to geojson we need a SpatialPolygonsDataFrame
    subdat.hs<-SpatialPolygonsDataFrame(subdat.hs, data=subdat_data.hs)
    
    # ----- set uo color pallette https://rstudio.github.io/leaflet/colors.html
    # Create a continuous palette function
    
    pal.1 <- colorNumeric(
      palette = "Blues",
      domain <- subdat_data.pk$Number
    )
    pal.2 <- colorNumeric(
      palette = "Reds",
      domain <- subdat_data.ele$number
    )
    pal.3 <- colorNumeric(
      palette = "Oranges",
      domain <- subdat_data.mid$number
    )
    pal.4 <- colorNumeric(
      palette = "Greens",
      domain <- subdat_data.k_8$number
    )
    pal.5 <- colorNumeric(
      palette = "Purples",
      domain <- subdat_data.hs$number
    )
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-73.983,40.7639,zoom = 12) %>% 
      addPolygons( data = subdat.pk,
                   stroke = T, weight=1,
                   fillOpacity = 0.6,
                   color = ~pal.1(Number),
                   label = ~paste0(ZIPCODE," Number of School: ",Number),
                   labelOptions = labelOptions(direction = "auto"),
                   highlight = highlightOptions(weight = 3,
                                                color = "blue",
                                                bringToFront = TRUE),
                   group = "Pre-K") %>%
      addPolygons( data = subdat.ele,
                   stroke = T, weight=1,
                   fillOpacity = 0.6,
                   color = ~pal.2(number),
                   label = ~paste0(ZIPCODE," Number of School: ",number),
                   labelOptions = labelOptions(direction = "auto"),
                   highlight = highlightOptions(weight = 3,
                                                color = "blue",
                                                bringToFront = TRUE),
                   group = "Elementary School")%>%
      addPolygons( data = subdat.mid,
                   stroke = T, weight=1,
                   fillOpacity = 0.6,
                   color = ~pal.3(number),
                   label = ~paste0(ZIPCODE," Number of School: ",number),
                   labelOptions = labelOptions(direction = "auto"),
                   highlight = highlightOptions(weight = 3,
                                                color = "blue",
                                                bringToFront = TRUE),
                   group = "Middle School")%>%
      addPolygons( data = subdat.k_8,
                   stroke = T, weight=1,
                   fillOpacity = 0.6,
                   color = ~pal.4(number),
                   label = ~paste0(ZIPCODE," Number of School: ",number),
                   labelOptions = labelOptions(direction = "auto"),
                   highlight = highlightOptions(weight = 3,
                                                color = "blue",
                                                bringToFront = TRUE),
                   group = "K-8 School")%>%
      addPolygons( data = subdat.hs,
                   stroke = T, weight=1,
                   fillOpacity = 0.6,
                   color = ~pal.5(number),
                   label = ~paste0(ZIPCODE," Number of School: ",number),
                   labelOptions = labelOptions(direction = "auto"),
                   highlight = highlightOptions(weight = 3,
                                                color = "blue",
                                                bringToFront = TRUE),
                   group = "High School")%>%
      addLayersControl(
        baseGroups = c("Pre-K", "Elementary School","Middle School",
                       "K-8 School","High School"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  

####################################### Pre-K School ###########################################
  # pre-k map
  output$pre_k_map <- renderLeaflet({
    if(input$pre_k_year=="2016"){
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% pk2016$Zipcode)
      subdat <- spTransform(selZip, CRS("+init=epsg:4326"))
      subdat_data=subdat@data[,c("ZIPCODE","POPULATION")]
      subdat.rownames=rownames(subdat_data)
      subdat_data$ZIPCODE <- as.character(subdat_data$ZIPCODE)
      pk2016$Zipcode  <- as.character(pk2016$Zipcode)
      subdat_data=
        subdat_data%>%left_join(pk2016, by=c("ZIPCODE" = "Zipcode"))
      rownames(subdat_data)=subdat.rownames
      subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
      pal.1 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$Enrollment
      )
      pal.2 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$CLASS.Emotional.Support.Score
      )
      pal.3 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$CLASS.Instructional.Support.Score
      )
      pal.4 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$Total.ECERS
      )
      
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(-73.983,40.7639,zoom = 12) %>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.1(Enrollment),
                     label = ~paste0(ZIPCODE," \n Enrollment: ",Enrollment),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "Enrollment") %>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.2(CLASS.Emotional.Support.Score),
                     label = ~paste0(ZIPCODE," Emotional Support: ",CLASS.Emotional.Support.Score),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "Emotional Support Score")%>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.3(CLASS.Instructional.Support.Score),
                     label = ~paste0(ZIPCODE," Instructional Support: ",CLASS.Instructional.Support.Score),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "Instructional Support")%>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.4(CLASS.Instructional.Support.Score),
                     label = ~paste0(ZIPCODE," ECERS Score: ",Total.ECERS),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "ECERS Score")%>%
        addLayersControl(
          baseGroups = c("Enrollment", "Emotional Support","Instructional Support","ECERS Score"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    } else if(input$pre_k_year=="2017"){
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% pk2017$Zipcode)
      subdat <- spTransform(selZip, CRS("+init=epsg:4326"))
      subdat_data=subdat@data[,c("ZIPCODE","POPULATION")]
      subdat.rownames=rownames(subdat_data)
      subdat_data$ZIPCODE <- as.character(subdat_data$ZIPCODE)
      pk2017$Zipcode  <- as.character(pk2017$Zipcode)
      subdat_data=
        subdat_data%>%left_join(pk2017, by=c("ZIPCODE" = "Zipcode"))
      rownames(subdat_data)=subdat.rownames
      subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
      pal.1 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$Enrollment
      )
      pal.2 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$CLASS.Emotional.Support.Score
      )
      pal.3 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$CLASS.Instructional.Support.Score
      )
      pal.4 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$Total.ECERS
      )
      
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(-73.983,40.7639,zoom = 12) %>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.1(Enrollment),
                     label = ~paste0(ZIPCODE," \n Enrollment: ",Enrollment),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "Enrollment") %>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.2(CLASS.Emotional.Support.Score),
                     label = ~paste0(ZIPCODE," Emotional Support: ",CLASS.Emotional.Support.Score),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "Emotional Support Score")%>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.3(CLASS.Instructional.Support.Score),
                     label = ~paste0(ZIPCODE," Instructional Support: ",CLASS.Instructional.Support.Score),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "Instructional Support")%>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.4(CLASS.Instructional.Support.Score),
                     label = ~paste0(ZIPCODE," ECERS Score: ",Total.ECERS),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "ECERS Score")%>%
        addLayersControl(
          baseGroups = c("Enrollment", "Emotional Support","Instructional Support","ECERS Score"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    } else if(input$pre_k_year=="2018"){
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% pk2018$Zipcode)
      subdat <- spTransform(selZip, CRS("+init=epsg:4326"))
      subdat_data=subdat@data[,c("ZIPCODE","POPULATION")]
      subdat.rownames=rownames(subdat_data)
      subdat_data$ZIPCODE <- as.character(subdat_data$ZIPCODE)
      pk2018$Zipcode  <- as.character(pk2018$Zipcode)
      subdat_data=
        subdat_data%>%left_join(pk2018, by=c("ZIPCODE" = "Zipcode"))
      rownames(subdat_data)=subdat.rownames
      subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
      pal.1 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$Enrollment
      )
      pal.2 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$CLASS.Emotional.Support.Score
      )
      pal.3 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$CLASS.Instructional.Support.Score
      )
      pal.4 <- colorNumeric(
        palette = "Reds",
        domain <- subdat_data$Total.ECERS
      )
      
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(-73.983,40.7639,zoom = 12) %>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.1(Enrollment),
                     label = ~paste0(ZIPCODE," \n Enrollment: ",Enrollment),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "Enrollment") %>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.2(CLASS.Emotional.Support.Score),
                     label = ~paste0(ZIPCODE," Emotional Support: ",CLASS.Emotional.Support.Score),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "Emotional Support Score")%>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.3(CLASS.Instructional.Support.Score),
                     label = ~paste0(ZIPCODE," Instructional Support: ",CLASS.Instructional.Support.Score),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "Instructional Support")%>%
        addPolygons( data = subdat,
                     stroke = T, weight=1,
                     fillOpacity = 0.6,
                     color = ~pal.4(CLASS.Instructional.Support.Score),
                     label = ~paste0(ZIPCODE," ECERS Score: ",Total.ECERS),
                     labelOptions = labelOptions(direction = "auto"),
                     highlight = highlightOptions(weight = 3,
                                                  color = "blue",
                                                  bringToFront = TRUE),
                     group = "ECERS Score")%>%
        addLayersControl(
          baseGroups = c("Enrollment", "Emotional Support","Instructional Support","ECERS Score"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    } 
    

  
    
  })
 
########################################## Other School Map #####################################
   output$grade_map <- renderLeaflet({
    
    # data preparation
    if (input$`school type` == "Elementary"){
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(ele_zipcode$zipcode))
      # ----- Transform to EPSG 4326 - WGS84 (required)
      subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
      school_zipcode <- ele_zipcode
      school_detail <- ele_total
    }
    else if (input$`school type` == "Middle"){
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(mid_zipcode$zipcode))
      # ----- Transform to EPSG 4326 - WGS84 (required)
      subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
      school_zipcode <- mid_zipcode
      school_detail <- mid_total
    }
    else if (input$`school type` == "K-8"){
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(k_8_zipcode$zipcode))
      # ----- Transform to EPSG 4326 - WGS84 (required)
      subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
      school_zipcode <- k_8_zipcode
      school_detail <- k_8_total
    }
    else {
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(hs_zipcode$zipcode))
      # ----- Transform to EPSG 4326 - WGS84 (required)
      subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
      school_zipcode <- hs_zipcode
      school_detail <- hs_total
    }
    
    zipcode_year <- school_zipcode%>%
      filter(yr == as.numeric(input$gd_year))
    


    #### zip code polygon
    subdat_data=subdat@data[,c("ZIPCODE","POPULATION")]
    subdat.rownames=rownames(subdat_data)
    subdat_data$ZIPCODE <- as.character(subdat_data$ZIPCODE)
    zipcode_year$zipcode  <- as.character(zipcode_year$zipcode)
    subdat_data=
      subdat_data%>%left_join(zipcode_year, by=c("ZIPCODE" = "zipcode"))
    rownames(subdat_data)= subdat.rownames
    
    # ----- to write to geojson we need a SpatialPolygonsDataFrame
    subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
    
    # ----- set uo color pallette https://rstudio.github.io/leaflet/colors.html
    # Create a continuous palette function
    
    pal.1 <- colorNumeric(
      palette = "Reds",
      domain <- subdat_data$enroll
    )
    pal.2 <- colorNumeric(
      palette = "Reds",
      domain <- subdat_data$SASS.mean
    )
    pal.3 <- colorNumeric(
      palette = "Reds",
      domain <- subdat_data$RIES.mean
    )
    
    
    
 

    
    
    
################# Map Drawing 
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-73.983,40.7639,zoom = 12) %>% 
      addPolygons( data = subdat,
                   stroke = T, weight=1,
                   fillOpacity = 0.6,
                   color = ~pal.1(enroll),
                   label = ~paste0(ZIPCODE," \n Enrollment: ",enroll),
                   labelOptions = labelOptions(direction = "auto"),
                   highlight = highlightOptions(weight = 3,
                                                color = "blue",
                                                bringToFront = TRUE),
                   group = "Enrollment") %>%
      addPolygons( data = subdat,
                   stroke = T, weight=1,
                   fillOpacity = 0.6,
                   color = ~pal.2(SASS.mean),
                   label = ~paste0(ZIPCODE," Student Achievement: ",SASS.mean),
                   labelOptions = labelOptions(direction = "auto"),
                   highlight = highlightOptions(weight = 3,
                                                color = "blue",
                                                bringToFront = TRUE),
                   group = "Student Achievement")%>%
      addPolygons( data = subdat,
                   stroke = T, weight=1,
                   fillOpacity = 0.6,
                   color = ~pal.3(RIES.mean),
                   label = ~paste0(ZIPCODE," Rigorous Instruction: ",RIES.mean),
                   labelOptions = labelOptions(direction = "auto"),
                   highlight = highlightOptions(weight = 3,
                                                color = "blue",
                                                bringToFront = TRUE),
                   group = "Rigorous Instruction")%>%
      addLayersControl(
        baseGroups = c("Enrollment", "Student Achievement","Rigorous Instruction"),
        options = layersControlOptions(collapsed = FALSE)
      )  
      
    


  })
  
  observeEvent(input$s_submit,{
    if (input$`school type` == "Elementary"){
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(ele_zipcode$zipcode))
      # ----- Transform to EPSG 4326 - WGS84 (required)
      subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
      school_zipcode <- ele_zipcode
      school_detail <- ele_total
    }
    else if (input$`school type` == "Middle"){
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(mid_zipcode$zipcode))
      # ----- Transform to EPSG 4326 - WGS84 (required)
      subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
      school_zipcode <- mid_zipcode
      school_detail <- mid_total
    }
    else if (input$`school type` == "K-8"){
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(k_8_zipcode$zipcode))
      # ----- Transform to EPSG 4326 - WGS84 (required)
      subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
      school_zipcode <- k_8_zipcode
      school_detail <- k_8_total
    }
    else {
      selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.character(hs_zipcode$zipcode))
      # ----- Transform to EPSG 4326 - WGS84 (required)
      subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
      school_zipcode <- hs_zipcode
      school_detail <- hs_total
    }
    
    
    
    ## zip code slected
    if(as.numeric(input$zip_s) %in% as.numeric(school_detail$X.zip)){
      selected_zip <- as.numeric(input$zip_s)
    }else{
      leafletProxy('grade_map') %>% 
        clearMarkers()
      return()}
    
    ## number of school displayed
    school_number <- as.numeric(input$s_number)

    ## filter based on zip and number
    school_detail <- school_detail %>%
       filter(X.zip. == selected_zip, year == 2018) %>% 
       arrange(desc(`Student Achievement - Section Score` + `Rigorous Instruction - Element Score`))
     
    if (nrow(school_detail) > school_number) {
      school_detail <- school_detail[1:school_number,]
    }
    
    print(school_detail$lon)
    leafletProxy('grade_map') %>% 
      clearMarkers() %>%  
      addAwesomeMarkers(~lon, ~lat,data = school_detail,
                        label = ~paste0(`School Name`))
      
    

  })
  
  

})


