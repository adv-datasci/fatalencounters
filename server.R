library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

#Must be installed from github
#devtools::install_github('bhaskarvk/leaflet.extras')
library(leaflet.extras)

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

#12/9
#add pop-ups with victim information

#12/17K
# Finished UI to allow user to choose what variable to put in
#           one histogram
#Added heatmap, edited data explorer, added intro

#12/18L
# Finished UI to allow user to choose which demographics
#           to put on the map

########## Set working directory #####

#setwd("your/directory/here")

# Read in the data
cleantable <-readRDS(file.path("data","processed_data","clean_fatal_dataset.RDS"))

#There are multiple shootings at the same zip codes, so add
#random noise to ensure that the markers don't overlap
cleantable$lat<-jitter(cleantable$lat)
cleantable$long<-jitter(cleantable$long) 

#Copy to modify when user specifies certain demographics
cleantable_unmodified<-cleantable

function(input, output, session) {
  ## Interactive Map ###########################################
  
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
  
  #################
  #Select variables to include in map
  #################
  
  #updateSelectizeInput(session, "selectize", choices=levels(factor(cleantable$race)), selected=choices[selectionIndex], 
  #                     server = TRUE)
  nValues <- 1002
  choices <- as.character(1:nValues)
  selectionIndex <- 1000
  
  observe({
    colorBy <- input$color
    if (colorBy == "race") {
      updateSelectizeInput(session, "selectize",
                           choices=levels(factor(cleantable$race)), selected=choices[selectionIndex], 
                           server = TRUE)
    } else if (colorBy == "sex") {
      updateSelectizeInput(session, "selectize",
                           choices=levels(factor(cleantable$sex)), selected=choices[selectionIndex], 
                           server = TRUE)
    } else if (colorBy == "age") {
      updateSelectizeInput(session, "selectize",
                           choices=c("< 1 year","1 - 4 years", "5 - 9 years",
                                     "10 - 14 years","15 - 19 years",
                                     "20 - 24 years","25 - 34 years",
                                     "35 - 44 years","45 - 54 years",
                                     "55 - 64 years","65 - 74 years",
                                     "75 - 84 years","85+ years"
                           )
                           , selected=choices[selectionIndex], 
                           server = TRUE)
    } else if (colorBy == "cause") {
      updateSelectizeInput(session, "selectize",
                           choices=levels(factor(cleantable$cause)), selected=choices[selectionIndex], 
                           server = TRUE)
    }
  })
  
  
  subsetData <- reactive({
    selected<-input$selectize
    
    if (is.na(selected)){
      return(cleantable)
    } else {
      a <- subset(cleantable, category == selected)
      return(a)
    }
  })
  
  
  #Output histograms in the user interface
  output$plot1 <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    if (input$hist == "race") {
      barplot(prop.table(table(zipsInBounds()$race)),
              main = "Racial demographics of decedents",
              ylab = "Race",
              cex.names=0.8,
              horiz = TRUE,
              xlab = "Proportion",
              col = '#00DD00',
              border = 'white')
    } else if (input$hist == "sex") {
      barplot(prop.table(table(zipsInBounds()$sex)),
              #breaks = centileBreaks,
              main = "Sex of decedents",
              ylab = "Percentage",
              cex.names=0.8,
              ylim = c(0,1),
              xlab = "Sex",
              col = '#00DD00',
              border = 'white')
    } else if (input$hist == "age") {
      barplot(prop.table(table(zipsInBounds()$agerng)),
              #breaks = centileBreaks,
              main = "Age profiles of decedents",
              horiz = TRUE,
              ylab = "Age groups",
              cex.names=0.8,
              xlab = "Proportion",
              col = '#00DD00',
              border = 'white')
    } else if (input$hist == "cause") {
      barplot(prop.table(table(zipsInBounds()$cause)),
              #breaks = centileBreaks,
              main = "Cause of death",
              horiz= TRUE, 
              ylab = "Cause",
              cex.names=0.8,
              xlab = "Proportion",
              col = '#00DD00',
              border = 'white')
    } else if (input$hist == "year") {
      barplot(prop.table(table(zipsInBounds()$year)),
              #breaks = centileBreaks,
              main = "Year breakdown of deaths",
              ylab = "Percentage",
              cex.names=0.8,
              xlab = "Year",
              col = '#00DD00',
              border = 'white')
    }
    
  })
  
  # output$plot2 <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   barplot(prop.table(table(zipsInBounds()$cause)),
  #           #breaks = centileBreaks
  #           main = "Cause of death",
  #           ylab = "Percentage",
  #           cex.names=0.8,
  #           ylim = c(0,1),
  #           xlab = "Cause of death",
  #           #xlim = range(allzips$centile),
  #           col = '#00DD00',
  #           border = 'white')
  # })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    #selectedFactors<-input$selectize
    
    #colorBy <- "sex"
    #selectedFactors<-c("Male","Transgender")
    #sprintf(colorBy,'%in%','selectedFactors')
    
    #paste()
    #eval(parse(paste(colorBy,'%in%','selectedFactors')))
    
    #cleantable<-subset(cleantable_unmodified, race %in% selectedFactors)
    #cleantable<-subset(cleantable_unmodified,paste(colorBy,'%in%','selectedFactors'))))
    
    
    #custom_palette <-trimws(c("#8c510a ", "#d8b365 ", "#f6e8c3 ", "#c7eae5 ", "#5ab4ac ", "#01665e "))
    
    if (colorBy == "race") {
      colorData <- factor(cleantable$race)
      pal <- colorFactor(palette = "Dark2", colorData)
      
      #Change this to specify each race as a more intuitive color?
      #pal <- c("black","white","yellow","brown","red", "orange", "green")
    } else if (colorBy == "cause") {
      colorData <- factor(cleantable$cause)
      pal <- colorFactor("Dark2", colorData)
      
    } else if (colorBy == "sex") {
      colorData <- factor(cleantable$sex)
      pal <- colorFactor("Dark2", colorData)
      
    } else if (colorBy == "age") {
      colorData <- factor(cleantable$agerng, 
                          levels<-
                            c("< 1 year","1 - 4 years", "5 - 9 years",
                              "10 - 14 years","15 - 19 years",
                              "20 - 24 years","25 - 34 years",
                              "35 - 44 years","45 - 54 years",
                              "55 - 64 years","65 - 74 years",
                              "75 - 84 years","85+ years"
                            )
      )
      
      pal <- colorFactor("viridis", colorData)
      
    } else if (colorBy == "year") {
      colorData <- factor(cleantable$year)
      pal <- colorFactor("Dark2", colorData)
      
    }
    
    #output$table1 <- renderTable(df_subset())
    
    #Draws the map and adds the legend
    leafletProxy("map", data = cleantable) %>%
      #leafletProxy("map") %>%
      clearShapes() %>%
      addCircleMarkers( ~long, ~lat, radius=3, layerId=~name,                       
                        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  showZipcodePopup <- function(name, lat, lng) {
    selectedZip <- cleantable[cleantable$name == name,]
    content <- as.character(tagList(
      tags$strong("Name:", selectedZip$name),tags$br(),
      sprintf("%s, %s %s",
              selectedZip$city, selectedZip$state, selectedZip$zipcode
      ), tags$br(),
      sprintf("Sex: %s", selectedZip$sex), tags$br(),
      sprintf("Age: %s", selectedZip$age), tags$br(),
      sprintf("Race: %s", selectedZip$race), tags$br(),
      sprintf("Cause of death: %s", selectedZip$cause)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = name)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  ############ Heat map ###############
  
  # Merge spatial df with downloaded ddata.
  leafmap <-readRDS(file.path("data","processed_data","heatmap_data2.RDS"))
  
  # rrcolorData <- leafmap$pminusdratio
  # pal <- colorQuantile("YlOrRd", unique(leafmap$pminusdratio), n = 6)
  
  binz <- c(0, 0.5, 0.99, 1.5, 2, 4, 8, 16, Inf)
  
  observe({
    rrBy <- input$rr
    
    if (rrBy == "pminusdratio") {
      rrcolorData <- leafmap$pminusdratio
      pal <- colorQuantile("YlOrRd", unique(leafmap$pminusdratio), n = 6)
    } else if (rrBy == "blackrr") {
      rrcolorData <- leafmap$blackrr
      pal <- colorBin("YlOrRd", rrcolorData, bins = binz)
    } else if (rrBy == "hisprr") {
      rrcolorData <- leafmap$hisprr
      pal <- colorBin("YlOrRd", rrcolorData, bins = binz)
    } else if (rrBy == "nativerr") {
      rrcolorData <- leafmap$nativerr
      pal <- colorBin("YlOrRd", rrcolorData, bins = binz)
    } else if (rrBy == "asianrr") {
      rrcolorData <- leafmap$asianrr
      pal <- colorBin("YlOrRd", rrcolorData, bins = binz)
    } 
    
    # Format popup data for leaflet map.
    popup_dat <- paste0("<strong>County: </strong>",
                        leafmap$NAME,
                        "<br><strong>Risk: </strong>",
                        round(rrcolorData, 2))
    
    # Create the map
    output$heatmap <- renderLeaflet({
      # Render final map in leaflet.
      leaflet(data = leafmap) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(rrcolorData),
                    fillOpacity = 0.8,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = popup_dat,
                    label = ~NAME,
                    group = 'counties') %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
        addLegend("bottomleft", pal=pal, values=rrcolorData, title= 'Relative Risk of Fatal Encounter',
                  layerId="riskLegend") %>%
        addSearchFeatures(
          targetGroups  = 'counties',
          options = searchFeaturesOptions(zoom=10, openPopup=TRUE)) %>%
        addResetMapButton() %>%
        addControl("<P><B>Rate of police-involved death</B> per 100,000 population</P><P>Since the year 2000</P><P>Search by county name</P>",
                   position='bottomright')
    })
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
        is.null(input$states) | state %in% input$states,
        is.null(input$cities) | city %in% input$cities,
        is.null(input$zipcodes) | zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '" data-zip="', zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
}



