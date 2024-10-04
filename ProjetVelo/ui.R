library(shiny)
library(shinythemes)
library(dygraphs)
library(DT)
library(plotly)
library(tidyverse)




# Define UI for application
fluidPage(
  #theme = shinytheme("cerulean"),
  # Ajout de styles personnalisés via CSS
  tags$head(
    tags$style(HTML("
      /* Couleur du bandeau */
      .navbar {
        background-color: #ec5030 !important;  /* Bordeaux clair */
      }
      
      /* Couleur du texte des onglets et du titre */
      .navbar-default .navbar-nav > li > a, 
      .navbar-default .navbar-brand {
        color: white !important;  /* Texte en blanc par défaut */
      }
      
      /* Couleur de l'onglet au survol */
      .navbar-default .navbar-nav > li > a:hover {
        background-color: #660000 !important;  /* Rouge foncé au survol */
        color: white !important;  /* Texte blanc au survol */
      }
      
      /* État de l'onglet actif (sélectionné) */
      .navbar-default .navbar-nav > .active > a {
        background-color: #660000 !important;  /* Rouge foncé tant que l'onglet est actif */
        color: white !important;  /* Texte blanc */
      }

      /* Garder la couleur de l'onglet actif au survol */
      .navbar-default .navbar-nav > .active > a:hover {
        background-color: #660000 !important;  /* Garder rouge foncé au survol */
        color: white !important;  /* Texte blanc */
      }

      /* Sous-onglets (dropdown) au survol */
      .navbar-default .navbar-nav .dropdown-menu > li > a:hover {
        background-color: #660000 !important;  /* Rouge foncé pour les sous-onglets */
        color: white !important;  /* Texte blanc */
      }

      /* Conserver la couleur rouge foncé pour le sous-onglet actif */
      .navbar-default .navbar-nav .dropdown-menu > .active > a {
        background-color: #660000 !important;  /* Rouge foncé pour le sous-onglet actif */
        color: white !important;  /* Texte blanc */
      }

      /* Pour maintenir l'onglet actif en rouge foncé, même lorsque des sous-onglets sont sélectionnés */
      .navbar-default .navbar-nav .dropdown.open > a {
        background-color: #660000 !important;  /* Rouge foncé quand le dropdown est ouvert */
        color: white !important;  /* Texte blanc */
      }
      
      /* Couleur des titres de section */
      h1, h2, h3, h4, h5, h6 {
        color: #662a2a !important;  /* Rouge foncé pour tous les titres */
      }
      
      /* Couleur des liens hypertexte */
    a {
      color: #651515  !important;  /* Rouge foncé pour les liens */
    }

    /* Couleur des liens au survol */
    a:hover {
      color: #ec5030 !important;  /* Bordeaux clair au survol */
    }
    
    
    
    /* Style du bouton principal */
      .stylish-button {
        background-color: #660000; /* Rouge foncé */
        color: white; /* Texte en blanc */
        border: none; /* Pas de bordure */
        border-radius: 5px; /* Coins arrondis */
        padding: 10px 20px; /* Espacement interne */
        font-size: 16px; /* Taille de police */
        cursor: pointer; /* Changer le curseur au survol */
        transition: background-color 0.3s; /* Animation douce */
      }

      /* Changer le style au survol */
      .stylish-button:hover {
        background-color: #ec5030; /* Bordeaux clair au survol */
      }

      /* Style du bouton pour la blague */
      .small-button {
        background-color: #660000; /* Rouge foncé */
        color: white; /* Texte en blanc */
        border: none; /* Pas de bordure */
        border-radius: 5px; /* Coins arrondis */
        padding: 5px 10px; /* Espacement interne réduit pour un bouton plus petit */
        font-size: 12px; /* Taille de police réduite */
        cursor: pointer; /* Changer le curseur au survol */
        transition: background-color 0.3s; /* Animation douce */
      }

      /* Changer le style au survol */
      .small-button:hover {
        background-color: #ec5030; /* Bordeaux clair au survol */
      }

      /* Positionner le conteneur de boutons en bas à gauche */
      .button-container {
        position: fixed; /* Positionnement fixe */
        bottom: 20px; /* 20 pixels du bas */
        left: 20px; /* 20 pixels de gauche */
        z-index: 1000; /* S'assurer que le conteneur est au-dessus des autres éléments */
      }

      /* Style pour le texte de la blague */
      .joke-text {
        margin-left: 10px; /* Espacement à gauche du texte de la blague */
        color: #660000; /* Couleur du texte de la blague */
        font-size: 14px; /* Taille de police pour le texte de la blague */
      }
    
    "))
  ),
  navbarPage(
    title = div(
      img(src = "logo.png", height = "40px", style = "margin-top: -10px;")  # Ajuste la hauteur et la position de l'image si nécessaire
    ),
    
    # Premier onglet - Présentation
    tabPanel(
      title = "Introduction",
      
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
),
    
    navbarMenu(title = "Description des données",
               tabPanel("Jeu de données",
                        titlePanel("Structure du jeu de données"),
                        fluidRow(
                          column(12,
                                 h3("Informations sur les données"),
                                 h4("Nombre de lignes :"),
                                 textOutput("nb_lignes"),
                                 h4("Variable Réponse :"),
                                 uiOutput("var_reponse"),   # Affichage de la variable réponse
                                 h4("Variables Explicatives Quantitatives :"),
                                 uiOutput("var_quant"),     # Affichage des variables quantitatives
                                 h4("Variables Explicatives Qualitatives :"),
                                 uiOutput("var_qual"),       # Affichage des variables qualitatives
                                 p(""),
                                 p(""),
                                 h3("Aperçu du jeu de données")
                          )
                        ), 
                        # Section Tableau de données
                        fluidRow(
                          column(12,
                                 DTOutput("data_table")
                          )
                        )
               ),
               tabPanel("Graphique",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("varSelect", 
                                        "Choisir la variable à analyser :",
                                        choices = names(df)[-c(1,2,15,16,19)]), 
                            dateRangeInput("dates", 
                                           "Sélectionner l'intervalle de dates (entre 2017-12-01 et 2018-11-30):", 
                                           start = "2017-12-01", 
                                           end = "2018-11-30",
                                           min = "2017-12-01", 
                                           max = "2018-11-30",
                                           format = "yyyy-mm-dd"), # Choix des dates
                            uiOutput("anova_result") # Affichage du résultat de l'ANOVA
                          ),
                          
                          mainPanel(
                            plotlyOutput("linePlot")  # Graphique
                          )
                        )),
    ),
    
    tabPanel(title = "Prediction",
             titlePanel("Prédiction du nombre de vélos loués"),
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choisir un fichier CSV",
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 tags$hr(),
                 actionButton("predict", "Faire la prédiction")
               ),
               
               mainPanel(
                 h3("Graphique des prédictions du nombre de vélos loués par heure"),
                 dygraphOutput("dygraph_predictions")
               )
             )
    ),
    
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
                        a(href = "https://www.linkedin.com/in/marine-lebreton-21421215a/", "Profil LinkedIn de Marine", target = "_blank")
                      )
               )
             ),
             tags$div(class = "button-container",
                      actionButton("joke_button", "Raconter une blague sur le vélo", class = "small-button"),
                      textOutput("joke_text", container = span, inline = TRUE)  # Affiche la blague à côté du bouton
             ),
             tags$div(
               style = "position: fixed; bottom: 20px; right: 20px; z-index: 1000;",  # Style pour la position
               img(src = "logoIARA.jpeg", style = "max-width: 200px; height: auto;")  # Garde le ratio d'aspect
             )
    )
  )
)