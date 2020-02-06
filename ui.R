### kbmorales
### UI for app
### kbmorales@protonmail.com


# Setup -------------------------------------------------------------------

library(leaflet)
library(spatial)
library(DT)

vars <- c(
  "Race" = "race",
  "Gender" = "sex",
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


# App ---------------------------------------------------------------------

navbarPage("Fatal Encounters", id="nav",
           
           tabPanel("Intro",
                    fluidPage(
                      div(id = "about", class = "card",  
                          includeMarkdown("./about.Rmd")
                      )
                    )
           ),
           

# Deaths map --------------------------------------------------------------


           tabPanel("Deaths",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS(file.path("code",
                                               "styles.css")),
                          includeScript(file.path("code",
                                                  "gomap.js"))
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls",
                                      class = "panel panel-default",
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = 60, 
                                      left = "auto",
                                      right = 20, 
                                      bottom = "auto",
                                      width = 330,
                                      height = "auto",
                                      
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
                                    
                                      plotOutput("plot1", height = 400)
                                      
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Fatal Encounters'), ' www.fatalencounters.org'
                        )
                    )
           ),


# Risk Map ----------------------------------------------------------------


           tabPanel("Risk Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS(file.path("code",
                                               "styles.css")),
                          includeScript(file.path("code",
                                                  "gomap.js"))
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("heatmap",
                                      width = "100%",
                                      height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls",
                                      class = "panel panel-default",
                                      fixed = TRUE,
                                      draggable = TRUE,
                                      top = 60,
                                      left = "auto",
                                      right = 20,
                                      bottom = "auto",
                                      width = 330,
                                      height = "auto",
                                      
                                      # Title of the sidebar
                                      h3("Risk Map"),
                                      
                                      # Descriptive text
                                      h5("Initial display is rank percentile of relative risk of police-involved death from 2000-2015, controlled for population."),
                                      
                                      # RR selector
                                      selectInput("rr",
                                                  "Relative Risk by Race",
                                                  rrraces,
                                                  selected = "None"),
                                      
                                      # Descriptive text
                                      h5("Relative risks are computed using white race as the comparison."),
                                      h5("For counties to display data, data must be available for both race groups.")
                                    
                                      ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ',
                                 tags$em('Fatal Encounters'),
                                 ' www.fatalencounters.org'
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


# Data explorer -----------------------------------------------------------


           tabPanel("Data explorer",
                    
                    dataTableOutput("ziptable")
           ),
           
           conditionalPanel("false",
                            icon("crosshair"))
)