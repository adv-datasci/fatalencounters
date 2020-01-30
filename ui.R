### kbmorales
### UI for app
### kbmorales@protonmail.com

library(leaflet)
library(spatial)

vars <- c(
  "Race" = "race",
  "Sex" = "sex",
  "Age" = "age",
  "Cause of death" = "cause",
  "Year" = "year"
)

rrraces <- c(
  "None" = "pminusdratio",
  "Black vs. White" = "blackrr",
  "Hispanic vs. White" = "hisprr",
  "Native vs. White" = "nativerr",
  "Asian vs. White" = "asianrr"
)

navbarPage("Fatal Encounters", id="nav",
           
           tabPanel("Intro",
                    fluidPage(
                      div(id = "about", class = "card",  
                          includeMarkdown("./about.Rmd")
                      )
                    )
           ),
           tabPanel("Deaths",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      #Title of the sidebar
                                      h3("Mapping Individual Deaths"),
                                      
                                      #Various icon inputs
                                      selectInput("color", "Demographic selector", vars, selected = NULL),
                                      
                                      #TO DO: Fix server.R so that this selector can work
                                      # selectizeInput(
                                      #   'selectize',
                                      #   'Select values to display on map',
                                      #   choices = NULL, multiple = TRUE ##############
                                      # ),
                                      
                                      selectInput("hist", "Histogram variable", vars, selected = "adultpop"),
                                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),
                                      
                                      #Bar plots below the user interface
                                      #plotOutput("histCentile", height = 200),
                                      #plotOutput("scatterCollegeIncome", height = 250)
                                      
                                      #plotOutput("plot1", height = 250),
                                      #plotOutput("plot2", height = 250)
                                      plotOutput("plot1", height = 400)
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Fatal Encounters'), ' www.fatalencounters.org'
                        )
                    )
           ),
           tabPanel("Risk Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("heatmap", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      # Title of the sidebar
                                      h3("Risk Map"),
                                      
                                      # Descriptive text
                                      h5("Initial display is overall risk from 2000-2015, controlled for population."),
                                      
                                      # RR selector
                                      selectInput("rr", "Relative Risk by Race", rrraces, selected = "None"),
                                      
                                      # Descriptive text
                                      h5("Relative risks are computed using white race as the comparison."),
                                      h5("For counties to display data, data must be available for both race groups.")
                                    
                                      ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Fatal Encounters'), ' www.fatalencounters.org'
                        )
                    )
           ),
           
           #                    div(class="outer",
           #                        tags$head(
           #                          # Include our custom CSS
           #                          includeCSS("styles.css"),
           #                          includeScript("gomap.js")
           #                        ),
           # If not using custom CSS, set height of leafletOutput to a number instead of percent
           #                        leafletOutput("heatmap", width="100%", height="100%")
           #                          leafletOutput("heatmap", width=1000, height=1000)
           #                    ),
           
           ## Data Explorer ###########################################
           
           tabPanel("Data explorer",
                    #TODO: State selector is broken and needs to be fixed
                    
                    # fluidRow(
                    #   column(3,
                    #          selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                    #   ),
                    #   column(3,
                    #          conditionalPanel("input.states",
                    #                           selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                    #          )
                    #   ),
                    #   column(3,
                    #          conditionalPanel("input.states",
                    #                           selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                    #          )
                    #   )
                    # ),
                    # hr(),
                    
                    DT::dataTableOutput("ziptable")
           ),
           
           conditionalPanel("false", icon("crosshair"))
)