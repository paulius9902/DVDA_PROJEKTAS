library(shiny)
ui <- fluidPage(
  titlePanel("Hello world"),
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
    )
  )
)
server <- function(input, output) {

}
shinyApp(ui = ui, server = server)
