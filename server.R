### Server side for app
### kbmorales
### kbmorales@proton

# Setup -------------------------------------------------------------------

library(sp)
library(rgeos)
library(rgdal)
# library(maptools)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(scales)
library(lattice)
library(tidyverse)
library(ggplot2)

function(input, output, session) {

# Base deaths map -----------------------------------------------------------
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        # urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}{r}.png"
        urlTemplate = "//{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
      ) %>%
      addCircleMarkers(data = fatal,
                       ~lon,
                       ~lat,
                       radius=3,
                       layerId=~name,                       
                       stroke=FALSE, 
                       fillOpacity=0.8, 
                       fillColor='#f5f5f5',
                       label = ~name, 
                       group = 'person') %>%
      ## Removing search--not working correctly
      # addSearchFeatures(
      #   targetGroups = 'person',
      #   options = searchFeaturesOptions(zoom=10, openPopup=TRUE)) %>%
      addResetMapButton() %>%
      ### Removing search option info
      # addControl("<P>You may search by a person's name.</P>",
      #            position='bottomright') %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  

# Death maps reactive data --------------------------------------------------
  
  ###
  ## Year range subset
  ###
  
  year_fatal <- reactive({
    year_rng_start <- input$range[1]
    year_rng_end <- input$range[2]
    
    subset(fatal,
           year >= year_rng_start &
             year <= year_rng_end)
  })
  
  ###
  ## Data in view
  ###
  
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(fatal[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(year_fatal(),
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2])
  })

# OLD MAYBE DELETE ---------------------------------------------------

  #updateSelectizeInput(session, "selectize", choices=levels(factor(cleantable$race)), selected=choices[selectionIndex], 
  #                     server = TRUE)
  # nValues <- 1002
  # choices <- as.character(1:nValues)
  # selectionIndex <- 1000
  
  # observe({
  #   colorBy <- input$color
  #   if (colorBy == "race") {
  #     updateSelectizeInput(session,
  #                          "selectize",
  #                          choices=levels(factor(fatal$race)), selected=choices[selectionIndex], 
  #                          server = TRUE)
  #   } else if (colorBy == "sex") {
  #     updateSelectizeInput(session, "selectize",
  #                          choices=levels(factor(fatal$sex)), selected=choices[selectionIndex], 
  #                          server = TRUE)
  #   } else if (colorBy == "age") {
  #     updateSelectizeInput(session, "selectize",
  #                          choices=c("< 1 year",
  #                                    "1 - 4 years",
  #                                    "5 - 9 years",
  #                                    "10 - 14 years",
  #                                    "15 - 19 years",
  #                                    "20 - 24 years",
  #                                    "25 - 34 years",
  #                                    "35 - 44 years",
  #                                    "45 - 54 years",
  #                                    "55 - 64 years",
  #                                    "65 - 74 years",
  #                                    "75 - 84 years",
  #                                    "85+ years"
  #                          ), 
  #                          selected=choices[selectionIndex], 
  #                          server = TRUE)
  #   } else if (colorBy == "cause") {
  #     updateSelectizeInput(session, "selectize",
  #                          choices=levels(factor(fatal$cause)), selected=choices[selectionIndex], 
  #                          server = TRUE)
  #   }
  # })
  
  
  # subsetData <- reactive({
  #   selected<-input$selectize
  #   
  #   if (is.na(selected)){
  #     return(fatal)
  #   } else {
  #     a <- subset(fatal,
  #                 category == selected)
  #     return(a)
  #   }
  # })
  

# Histograms ---------------------------------------------------------------
  
  #Output histograms in the user interface
  output$plot1 <- renderPlot({
    
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    # Otherwise output by selection
    if (input$color == "race") {
      ggplot(zipsInBounds(),
             aes(x = race,
                 fill = race)) + 
        geom_bar() + 
        coord_flip() +
        labs(x = "",
             y = "Count") +
        scale_x_discrete(limits = rev(levels(fatal$race))) +
        scale_fill_viridis_d() +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text=element_text(size=14)) 
    } else if (input$color == "sex") {
      ggplot(fatal,
             aes(x = sex,
                 fill = sex)) + 
        geom_bar() + 
        coord_flip() +
        labs(x = "",
             y = "Count") +
        scale_fill_viridis_d() +
        scale_x_discrete(limits = rev(levels(fatal$sex))) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text=element_text(size=14)) 
    } else if (input$color == "age") {
      ggplot(zipsInBounds(),
             aes(x = agerng,
                 fill = agerng)) + 
        geom_bar() + 
        coord_flip() +
        labs(x = "",
             y = "Count") +
        scale_fill_viridis_d() +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text=element_text(size=14)) 
    } else if (input$color == "cause") {
      ggplot(zipsInBounds(),
             aes(x = cause,
                 fill = cause)) + 
        geom_bar() + 
        coord_flip() +
        labs(x = "",
             y = "Count") +
        scale_fill_viridis_d() +
        scale_x_discrete(limits = c("Unknown",
                                    "Other",
                                    "Vehicle",
                                    "Tasered",
                                    "Gunshot")
        ) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text=element_text(size=14)) 
    } 
    
  })
  

# Update markers ----------------------------------------------------------
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  ## Update markers according to year selection
  
  observe({
    # years <- input$range
    leafletProxy("map") %>%
      #leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers( ~lon, ~lat,
                        data = year_fatal(),
                        radius=3,
                        layerId=~name,
                        stroke=FALSE,
                        fillOpacity=0.8,
                        fillColor='#f5f5f5',
                        label = ~name,
                        group = 'person')
                        
  })
  
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
    if (colorBy == "none") {
      leafletProxy("map", 
                   data = fatal) %>%
        #leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers( ~lon, ~lat, 
                          data = year_fatal(),
                          radius=3, 
                          layerId=~name,                       
                          stroke=FALSE, 
                          fillOpacity=0.8, 
                          fillColor='#f5f5f5',
                          label = ~name, 
                          group = 'person')
    } else {
      
      if (colorBy == "race") {
      colorData <- fatal$race
      
      } else if (colorBy == "cause") {
        colorData <- fatal$cause
        
      } else if (colorBy == "sex") {
        colorData <- fatal$sex
        
      } else if (colorBy == "age") {
        colorData <- fatal$agerng
      }
      
      pal <- colorFactor("viridis", 
                         colorData)
      
      #output$table1 <- renderTable(df_subset())
      
      #Draws the map and adds the legend
      leafletProxy("map", 
                   data = fatal) %>%
        #leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers( ~lon, ~lat, 
                          data = year_fatal(),
                          radius=3, 
                          layerId=~name,                       
                          stroke=FALSE, 
                          fillOpacity=0.8, 
                          fillColor=pal(colorData), 
                          label = ~name, 
                          group = 'person') 
      
      ## To readd legend (debugging)
      
        # addLegend("bottomleft", 
        #           pal=pal, 
        #           values=colorData, 
        #           title=paste0(
        #             toupper(substring(colorBy, 1,1)),
        #             substring(colorBy, 2)
        #             ),
        #           layerId="colorLegend")
    
    }
    
  })
  
  showZipcodePopup <- function(name, lat, lng) {
    
    ## Prepare selection
    selectedZip <- fatal[fatal$name == name,] %>% 
      mutate(img = ifelse(!is.na(url_name),
                          paste("<center><img src='", 
                                url_name,
                                "' height=",
                                "'100'></img></center>",
                                sep=""),
                          "No image on file"),
             source = paste("<a href='",
                            news_link,
                            "'>Link</a>",
                            sep="")
      )
    
    ## Display
    content <- as.character(tagList(
      tags$strong(selectedZip$name),tags$br(),
      HTML(selectedZip$img), tags$br(),
      sprintf("%s, %s %s",
              selectedZip$city, selectedZip$state, selectedZip$zipcode
      ), tags$br(),
      sprintf("Sex: %s", selectedZip$sex), tags$br(),
      sprintf("Age: %s", selectedZip$age), tags$br(),
      sprintf("Race: %s", selectedZip$race), tags$br(),
      sprintf("Cause of death: %s", selectedZip$cause), tags$br(),
      sprintf("Year: %s", selectedZip$year)
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
  

# Heat map ----------------------------------------------------------------

  # Merge spatial df with downloaded ddata.
  leafmap <-readRDS(file.path("data",
                              "processed_data",
                              "heatmap_data3.RDS"))
  
  # rrcolorData <- leafmap$pminusdratio
  # pal <- colorQuantile("YlOrRd", unique(leafmap$pminusdratio), n = 6)
  
  binz <- c(0, 0.5, 0.99, 1.5, 2, 4, 8, 16, Inf)
  
  observe({
    rrBy <- input$rr
    
    if (rrBy == "pminusdratio") {
      rrcolorData <- leafmap$pminusdratio
      pal <- colorQuantile("YlOrRd", unique(leafmap$pminusdratio), n = 6, na.color = "transparent")
    } else if (rrBy == "blackrr") {
      rrcolorData <- leafmap$blackrr
      pal <- colorBin("YlOrRd", rrcolorData, bins = binz, na.color = "transparent")
    } else if (rrBy == "hisprr") {
      rrcolorData <- leafmap$hisprr
      pal <- colorBin("YlOrRd", rrcolorData, bins = binz, na.color = "transparent")
    } else if (rrBy == "nativerr") {
      rrcolorData <- leafmap$nativerr
      pal <- colorBin("YlOrRd", rrcolorData, bins = binz, na.color = "transparent")
    } else if (rrBy == "asianrr") {
      rrcolorData <- leafmap$asianrr
      pal <- colorBin("YlOrRd", rrcolorData, bins = binz, na.color = "transparent")
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
        addTiles(urlTemplate = "//{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png") %>%
        addPolygons(fillColor = ~pal(rrcolorData),
                    fillOpacity = 0.6,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = popup_dat,
                    label = ~NAME,
                    group = 'counties') %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
        addLegend("bottomleft",
                  pal=pal, 
                  values=rrcolorData, 
                  title= 'Relative Risk',
                  layerId="riskLegend") %>%
        addSearchFeatures(
          targetGroups  = 'counties',
          options = searchFeaturesOptions(zoom=10, openPopup=TRUE)) %>%
        addResetMapButton() %>%
        addControl("<P>Search by county name</P>",
                   position='bottomright')
    })
  })
  
  # Data Explorer -----------------------------------------------------------
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(fatal, State %in% input$states) %>%
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
      fatal %>%
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
  
  output$fe_table <- DT::renderDataTable({
    
    df <- fatal %>%
      arrange(desc(date)) %>%
      mutate(img = ifelse(!is.na(url_name),
                            paste("<img src='", 
                                  url_name,
                                  "' height=",
                                  "'100'></img>",
                                  sep=""),
                            NA),
             source = paste("<a href='",
                                   news_link,
                                   "'>Link</a>",
                                   sep="")
             )
    
    
      
    
  #TODO: Allows user to go to selected row on the interactive map
  #  Needs to be fixed
  #mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '" data-zip="', zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    
    df<-df %>%
      select(
        "Name" = name,
        "Image" = img,
        "Date" = date,
        "Age" = age,
        "Gender" = sex,
        "Race" = race,
        "City" = city, 
        "State" = state,
        "County" = county,
        "Agency" = agency,
        "Cause" = cause,
        "Description" = description,
        "Disposition" = official_description,
        "Source" = source
      )

  
  #TODO: Fix this line; supposed to go to the relevant dot on the map
      #mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '" data-zip="', zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  }
  #TODO: Has no effect, but should make description column wider
  #,
  # options = list(
  #   autoWidth = TRUE,
  #   scrollX=TRUE,
  #   columnDefs = list(list(width = '500px', targets = 12))
  # )
  )
  
}
