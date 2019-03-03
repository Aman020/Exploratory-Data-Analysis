library(shiny)

ui <- pageWithSidebar(

  # App title ----
  headerPanel("FLU ANALUSIS"),

  # Sidebar panel for inputs ----
  sidebarPanel(),

  # Main panel for displaying outputs ----
  mainPanel()
)


server <- function(input, output) {

}

shinyApp(ui, server)