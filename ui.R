library(leaflet)
library(spatial)

#EDITS:
#11/26:
# Delete code for data explorer tab
# Change names of variables


# Choices for drop-downs

#11

vars <- c(
  "Race" = "race",
  "Sex" = "sex",
  "Age" = "age",
  "Cause of death" = "cause"
)

navbarPage("Fatal Encounters", id="nav",

           tabPanel("Interactive map",
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
                                      h2("Map explorer"),

                                      #Various icon inputs
                                      selectInput("color", "Color", vars),
                                      #selectInput("size", "Size", vars, selected = "adultpop"),
                                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),

                                      #Bar plots below the user interface
                                      #plotOutput("histCentile", height = 200),
                                      #plotOutput("scatterCollegeIncome", height = 250)

                                      plotOutput("plot1", height = 250),
                                      plotOutput("plot2", height = 250)
                        ),

                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Fatal Encounters'), ' www.fatalencounters.org'
                        )
                    )
           ),
           tabPanel("Heat map",
#                    div(class="outer",
#                        tags$head(
#                          # Include our custom CSS
#                          includeCSS("styles.css"),
#                          includeScript("gomap.js")
#                        ),
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
#                        leafletOutput("heatmap", width="100%", height="100%")
                          leafletOutput("heatmap", width=1000, height=1000)
                    ),

           ## Data Explorer ###########################################

           tabPanel("Data explorer",
                    fluidRow(
                      column(3,
                             selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(1,
                             numericInput("minScore", "Min score", min=0, max=100, value=0)
                      ),
                      column(1,
                             numericInput("maxScore", "Max score", min=0, max=100, value=100)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           ),

           conditionalPanel("false", icon("crosshair"))
)
