library(shiny)
library(shinythemes)
require(dygraphs)


#PROBLEMES : le graphique les bornes ne changent pas les graphiques


# Define UI for application
fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    title = "Projet Shiny",
    
    # Premier onglet - Présentation
    tabPanel(
      title = "Introduction",
      
      titlePanel("Introduction"),
      
      fluidRow(
        # Colonne de gauche pour le texte
        column(8,  # 8 sur 12 colonnes pour le texte
               h2("Contexte"),
               p("Face à la congestion routière et à la pollution croissante, 
                 de nombreuses métropoles investissent dans des moyens de 
                 transports plus durables, comme le vélo en libre service. 
                 C'est notamment le cas de Séoul qui, a rejoint la tendance en 
                 2015 avec son programme Seoul Bike, plus connu sous le nom de 
                 Ttareungyi. Ce programme permet aux habitants et aux visiteurs 
                 de louer des vélos à travers la ville à des prix abordables, 
                 favorisant un mode de transport écologique et réduisant la 
                 dépendance aux véhicules motorisés."),
               p("Depuis sa mise en place, Seoul Bike a rencontré un succès 
                 franc et compte aujourd'hui plus de 800 stations à travers la 
                 ville. Une certaine logistique est donc indispensable pour 
                 assurer le bon fonctionnement du service et répondre à la 
                 demande croissante des utilisateurs. Prévoir le nombre de vélos 
                 nécessaires et identifier les facteurs clés qui influencent la 
                 demande de location peut grandement aider à fournir à la ville 
                 une offre stable de vélos."),
               p("Le jeu de données utilisé pour répondre à cette problématique 
                 est composé de 14 variables : 1 variable réponse (le nombre de 
                 vélos loués), 5 variables qualitatives liées à la temporalité 
                 et 8 variables quantitatives liées aux conditions météorologiques.")
        ),
        
        # Colonne de droite pour l'image
        column(4,  # 4 sur 12 colonnes pour l'image
               img(src = "photo.jpg", style = "max-width: 100%; height: auto; margin-top: 50px;")
        )
      ),
      
      # Deuxième section - Problématique
      h2("Problématique"),
      p("Faut qu'on trouve une problématique."),
      
      # Texte de séparation ou signature
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
             "..............NOBODY_LOL..................."),
    tabPanel("Rencontrez notre équipe",
    
    fluidRow(
      # Carte pour le premier membre de l'équipe
      column(4,  
             div(
               class = "card",
               style = "padding: 20px; text-align: center;",
               h3("Timéo Baudat"),
               img(src = "tim.jpeg", height = "150px", width = "150px", style = "border-radius: 50%;"),
               p(""),
               a(href = "https://www.linkedin.com/in/timeo-baudat/", "Profil LinkedIn de Timéo", target = "_blank")
             )
      ),
      
      # Carte pour le deuxième membre de l'équipe
      column(4,  
             div(
               class = "card",
               style = "padding: 20px; text-align: center;",
               h3("Élise Lonchampt"),
               img(src = "élise.jpeg", height = "150px", width = "150px", style = "border-radius: 50%;"),
               p(""),
               a(href = "https://www.linkedin.com/in/%C3%A9lise-lonchampt-232705235/", "Profil LinkedIn d'Élise", target = "_blank")
             )
      ),
      
      # Carte pour le troisième membre de l'équipe
      column(4,  
             div(
               class = "card",
               style = "padding: 20px; text-align: center;",
               h3("Marine Lebreton"),
               img(src = "marine.jpeg", height = "150px", width = "150px", style = "border-radius: 50%;"),
               p(" "),
               a(href = "https://www.linkedin.com/in/marine-lebreton-21421215a/", "Profil LinkedIn", target = "_blank")
               )
             )
      )
    )
  )
)

