library(shiny)

# An empty morphological field that is editable (can be filled)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("My First Morphological Field"),
  fluidRow(
    column(
      12,
      morphr::morphFieldOutput("morphfield")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  morphr::installMorphField(input, output, id = "morphfield",
                            editable = TRUE)

}

# Run the application
shinyApp(ui = ui, server = server)
