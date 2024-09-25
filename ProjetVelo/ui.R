library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  navbarPage(
    title = "My first app",
    tabPanel(title = "Presentation",
             "................COUCOU C4EST TIM................."),
    
    navbarMenu(title = "Description des données",
             tabPanel("Graphique",
                      ".......C'est Marine................."),
             tabPanel("AFM",
                      "......coucou..................")
              ),
    
    tabPanel(title = "Prediction",
             ".................................")
  )
)

