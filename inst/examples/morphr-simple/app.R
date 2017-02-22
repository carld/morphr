#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI
ui <- fluidPage(

  # Application title
  titlePanel("My First Morphological Field"),

  fluidRow(
    morphr::morphFieldOutput("morphfield")
  )

)

# Define server logic
server <- function(input, output) {

  param_values <- list(
    "Parameter A" = c("A1", "A2", "A3", "A4"),
    "Parameter B" = c("B1", "B2", "B3", "B4"),
    "Parameter C" = c("C1", "C2")
  )

  specific_configurations <- list(
    "Parameter A" = list(
      "A1" = list(
        "Parameter B" = c("B2", "B3"),
        "Parameter C" = "C1"
      ),
      "A2" = list(
        "Parameter B" = "B4",
        "Parameter C" = "C2"
      ),
      "A3" = list(
        "Parameter B" = "B1",
        "Parameter C" = "C1"
      ),
      "A4" = list(
        "Parameter B" = "B3",
        "Parameter C" = "C2"
      )
    )
  )

  morphr::installMorphField(input, output, session, id = "morphfield",
                            param_values = param_values,
                            specific_configurations = specific_configurations)

}

# Run the application
shinyApp(ui = ui, server = server)

