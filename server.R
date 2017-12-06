library(sp)
library(rgeos)
library(rgdal)
library(maptools)

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

#EDITS
#11/26:
#delete data explorer
#delete size changer of points

#12/1:
#add back data explorer
#add in tab for heat map
#add in additional packages for heat map

#12/3
#incorporate cleaned datasest

########## Set working directory #####

#setwd("your/directory/here")


########## Not sure if we need this piece of code below

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]

#cleantable_sm <- cleantable[sample.int(nrow(cleantable), 10000),]

#allzips is about 30k observations, cleantable is 22ks
#final table is 13.5k observations

function(input, output, session) {

  ## Interactive Map ###########################################
  cleantable <-readRDS(file.path("data","processed_data","clean_fatal_dataset.RDS"))

    # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now

  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(cleantable[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(cleantable,
           lat >= latRng[1] & lat <= latRng[2] &
             long >= lngRng[1] & long <= lngRng[2])
  })

  #Output histograms in the user interface
  output$plot1 <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    barplot(prop.table(table(zipsInBounds()$race)),
            #breaks = centileBreaks,
            main = "Racial demographics of fatal encounters",
            ylab = "Percentage",
            cex.names=0.8,
            ylim = c(0,1),
            xlab = "Racial groups",
            col = '#00DD00',
            border = 'white')
  })

  output$plot2 <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    barplot(prop.table(table(zipsInBounds()$cause)),
            #breaks = centileBreaks
            main = "Cause of death",
            ylab = "Percentage",
            cex.names=0.8,
            ylim = c(0,1),
            xlab = "Cause of death",
            #xlim = range(allzips$centile),
            col = '#00DD00',
            border = 'white')
      })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

    #Colors Superzip points differently than non-Superzip
    #Could color differently by race or cause of death
    #If you want to specify specific colors instead of palette, need to draw
      #map with

    if (colorBy == "race") {
      colorData <- factor(cleantable$race)
      pal <- colorFactor("viridis", colorData)

      #Change this to specify each race as a more intuitive color?
      #pal <- c("black","white","yellow","brown","red", "orange", "green")
    } else if (colorBy == "cause") {
      colorData <- factor(cleantable$cause)
      pal <- colorFactor("viridis", colorData)

    } else if (colorBy == "sex") {
      colorData <- factor(cleantable$sex)
      pal <- colorFactor("viridis", colorData)

      } else if (colorBy == "age") {
      colorData <- cleantable[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }


    #Draws the map and adds the legend
    leafletProxy("map", data = cleantable) %>%
      clearShapes() %>%
      #addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
      #           stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%

      #Changed radius
      addCircleMarkers(~long, ~lat, radius=3, layerId=~zipcode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      #addMarkers(~long, ~lat, radius=1, layerId=~zipcode,
      #           stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })

  # Show a popup at the given location
  #Include information about the shooting and victim
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #                              selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }

  # # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #
  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })

############ Heat map ###############

  # Merge spatial df with downloaded ddata.
#  leafmap <- merge(us.map, county_dat, by=c("GEOID"))

  leafmap <-readRDS(file.path("data","processed_data","heatmap_data.RDS"))

  # Format popup data for leaflet map.
  popup_dat <- paste0("<strong>County: </strong>",
                      leafmap$NAME,
                      "<br><strong>Value: </strong>",
                      leafmap$pminusdratio)

  pal <- colorQuantile("YlOrRd", NULL, n = 9)

  # Create the map
  output$heatmap <- renderLeaflet({
    # Render final map in leaflet.
    leaflet(data = leafmap) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(unique(pminusdratio)),
                  fillOpacity = 0.8,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = popup_dat) %>%
    setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

###########################

# Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        #Score >= input$minScore,
        #Score <= input$maxScore,
        is.null(input$states) | state %in% input$states,
        is.null(input$cities) | city %in% input$cities,
        is.null(input$zipcodes) | zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '" data-zip="', zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
 }



