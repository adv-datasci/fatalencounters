### kbmorales
### UI for app
### kbmorales@protonmail.com


# Setup -------------------------------------------------------------------

library(leaflet)
library(spatial)
library(DT)

vars <- c(
  "None" = "none",
  "Race" = "race",
  "Gender" = "sex",
  "Age" = "age",
  "Cause of death" = "cause"
)

rrraces <- c(
  "None" = "death_rate",
  "Black vs. White" = "blackrr",
  "Hispanic vs. White" = "hisprr",
  "Native vs. White" = "nativerr",
  "Asian vs. White" = "asianrr"
)

## OLD
# rrraces <- c(
#   "None" = "pminusdratio",
#   "Black vs. White" = "blackrr",
#   "Hispanic vs. White" = "hisprr",
#   "Native vs. White" = "nativerr",
#   "Asian vs. White" = "asianrr"
# )


# App ---------------------------------------------------------------------

navbarPage("Fatal Encounters", 
           
           
           
           id="nav",
           theme = shinythemes::shinytheme("cosmo"),
           
           tabPanel("Intro",
                    fluidPage(
                      div(id = "about", class = "card",  
                          includeMarkdown("./about.Rmd")
                      )
                    )
           ),
           
           # Change slider color
           tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: black;
                                                  border-top: 1px solid #000039 ;
                                                  border-bottom: 1px solid #000039 ;}

                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: black }'
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
                        leafletOutput("map",
                                      width="100%",
                                      height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls",
                                      class = "panel panel-default",
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = 60, 
                                      left = "auto",
                                      right = 20, 
                                      bottom = "auto",
                                      width = 500,
                                      height = "auto",
                                      
                                      #Title of the sidebar
                                      h3("Mapping Individual Deaths\n"),
                                      
                                      # Input: Specification of range within an interval
                                      sliderInput("range", "Years:",
                                                  min = min(fatal$year, na.rm = T),
                                                  max = max(fatal$year, na.rm = T),
                                                  value = c(min(fatal$year, na.rm = T),
                                                            max(fatal$year, na.rm = T)),
                                                  step = 1,
                                                  width = "100%",
                                                  ticks = FALSE,
                                                  sep = "",
                                                  dragRange = FALSE
                                                  ),
                                      
                                      #Various icon inputs
                                      selectInput("color", 
                                                  "Demographic",
                                                  vars, 
                                                  selected = "none"),
                                      
                                      #TODO: Fix server.R so that this selector can work
                                      # selectizeInput(
                                      #   'selectize',
                                      #   'Select values to display on map',
                                      #   choices = NULL, multiple = TRUE ##############
                                      # ),
                                      
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
                                      h5("Initial display is deaths per 10,000 county population, according to 2010 US Census data."),
                                      
                                      h4("Relative Risk"),
                                      
                                      # RR selector
                                      selectInput("rr",
                                                  "Relative Risk by Race",
                                                  rrraces,
                                                  selected = "None"),
                                      # Descriptive text
                                      h5("Relative risks compare death rates between decedents by race, when known."),
                                      h5("White race is always the reference group."),
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
                    dataTableOutput("fe_table")
                    )
           
           # conditionalPanel("false",
           #                  icon("crosshair"))
)
