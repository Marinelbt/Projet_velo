library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  navbarPage(
    title = "My first app",
    tabPanel(title = "Presentation",
             "................................."),
    
    tabPanel(title = "Description des données",
             tabPanel("Graphique",
                      "........................"),
             tabPanel("AFM",
                      "........................")
              ),
    
    tabPanel(title = "Prediction",
             ".................................")
  )
)
