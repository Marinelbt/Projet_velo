library(shiny)
require(dygraphs)


#PROBLEMES : le graphique les bornes ne changent pas les graphiques


# Define UI for application
fluidPage(
  navbarPage(
    title = "My first app",
    tabPanel(title = "Presentation",
             "................MARINE................."),
    
    navbarMenu(title = "Description des données",
             tabPanel("Graphique",
                      ".......TIM.................",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("dates", 
                                         "Sélectionner l'intervalle de dates :", 
                                         start = "2017-12-01", 
                                         end = "2018-11-30",
                                         min = "2017-12-01", 
                                         max = "2018-11-30",
                                         format = "yyyy-mm-dd")
                        ),
                        
                        mainPanel(
                          dygraphOutput("linePlot")  # Utilisation de dygraph pour le graphique
                        )
                      )),
             tabPanel("AFM",
                      "......ELISE..................")
              ),
    
    tabPanel(title = "Prediction",
             "..............NOBODY_LOL...................")
  )
)

