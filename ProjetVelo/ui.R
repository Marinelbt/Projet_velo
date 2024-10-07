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
        background-color: #2C3E50 !important;  /* Bleu sombre */
      }
      
      /* Couleur du texte des onglets et du titre */
      .navbar-default .navbar-nav > li > a, 
      .navbar-default .navbar-brand {
        color: white !important;  /* Texte en blanc par défaut */
      }
      
      /* Couleur de l'onglet au survol */
      .navbar-default .navbar-nav > li > a:hover {
        background-color: #34495E !important;  /* Gris foncé au survol */
        color: white !important;  /* Texte blanc au survol */
      }
      
      /* État de l'onglet actif (sélectionné) */
      .navbar-default .navbar-nav > .active > a {
        background-color: #34495E !important;  /* Gris foncé tant que l'onglet est actif */
        color: white !important;  /* Texte blanc */
      }

      /* Garder la couleur de l'onglet actif au survol */
      .navbar-default .navbar-nav > .active > a:hover {
        background-color: #34495E !important;  /* Garder gris foncé au survol */
        color: white !important;  /* Texte blanc */
      }

      /* Couleur des sous-onglets (dropdown) */
      .navbar-default .navbar-nav .dropdown-menu > li > a:hover {
        background-color: #34495E !important;  /* Gris foncé pour les sous-onglets */
        color: white !important;  /* Texte blanc */
      }
      
      /* Conserver la couleur grise pour le sous-onglet actif */
      .navbar-default .navbar-nav .dropdown-menu > .active > a {
        background-color: #34495E !important;  /* Gris foncé pour le sous-onglet actif */
        color: white !important;  /* Texte blanc */
      }

      /* Titre des sections */
      h1, h2, h3, h4, h5, h6 {
        color: #2C3E50 !important;  /* Bleu sombre */
      }
      
      /* Couleur des liens */
      a {
        color: #1ABC9C !important;  /* Vert menthe pour les liens */
      }

      /* Couleur des liens au survol */
      a:hover {
        color: #16A085 !important;  /* Vert foncé au survol */
      }
    
    
    
    /* Style du bouton principal */
      .stylish-button {
        background-color: #2C3E50; /* Bleu sombre */
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
        background-color: #34495E; /* Gris foncé au survol */
      }

      /* Style du bouton pour la blague */
      .small-button {
        background-color: #2C3E50; /* Bleu sombre */
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
        background-color: #ffc500; /* jaune au survol */
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
      style = "display: flex; justify-content: center; align-items: center; height: 60px; width: 100%;",
      img(src = "seoul_bike.png", height = "60px", style = "margin-top: -40px;")
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
               p(""),
               p("Le jeu de données utilisé pour répondre à cette problématique 
                 est composé de 14 variables : 1 variable réponse (le nombre de 
                 vélos loués), 5 variables qualitatives liées à la temporalité 
                 et 8 variables quantitatives liées aux conditions météorologiques.")
        ),
        
        # Colonne de droite pour l'image
        column(4,  # 4 sur 12 colonnes pour l'image
               img(src = "photo.jpg", style = "max-width: 100%; height: auto; margin-top: 50px;")
        )
        
      )
    ),
    
    navbarMenu(title = "Description des données",
               tabPanel("Jeu de données",
                        titlePanel("Structure du jeu de données"),
                        fluidRow(
                          column(8,  # Colonne pour les informations
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
                                 p("")
                          ),
                          column(4,  # Colonne pour l'image
                                 br(), br(),  # Ajoute deux sauts de ligne pour descendre l'image
                                 img(src = "seoul.webp", 
                                     alt = "Description de l'image", 
                                     style = "width: 100%; height: auto;")  
                          )
                        ), 
                        # Section Tableau de données
                        fluidRow(
                          column(12,
                                 h3("Aperçu du jeu de données"),
                                 DTOutput("data_table")
                          )
                        )
               ),
               tabPanel("Analyse des variables",
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("varSelectUI"),
                            conditionalPanel(
                              condition = "input.varSelect == 'Heure'",
                              selectInput("typeJour",
                                          "Choisir les jours à afficher :",
                                          choices = c("Semaine" = "Semaine","Week-end" = "Week-end","Semaine et week-end" = "Semaine et week-end"),
                                          selected = "Semaine et week-end")),
                            dateRangeInput("dates", 
                                           "Sélectionner l'intervalle de dates (entre 2017-12-01 et 2018-11-30):", 
                                           start = "2017-12-01", 
                                           end = "2018-11-30",
                                           min = "2017-12-01", 
                                           max = "2018-11-30",
                                           format = "yyyy-mm-dd"), # Choix des dates
                            uiOutput("date_message"), # Affichage du message
                            div(uiOutput("anova_result"), style = "margin-bottom: 20px;"), # Affichage du résultat de l'ANOVA
                            div(uiOutput("correlation"))), # Affichage du coeff de corrélation si quanti
                          
                          mainPanel(
                            plotlyOutput("linePlot")  # Graphique
                          )
                        ))
    ),
    
    tabPanel(
      title = "Prediction",
      titlePanel("Prédiction du nombre de vélos loués"),
      sidebarLayout(
        sidebarPanel(
          # Bouton pour télécharger le fichier modèle CSV
          downloadButton("download_model", "Télécharger le fichier modèle CSV"),
          
          # Texte explicatif entre l'exportation et l'importation
          tags$p("Le fichier CSV que vous importez doit contenir les mêmes variables que le fichier modèle. Si vous souhaitez plus d'information sur les variables du jeu de données, rendez vous dans l'onglet 'Jeu de données' de la partie 'Description des données'."),
          tags$p(""),
          # Bouton pour importer un fichier CSV
          fileInput("file1", "Choisir un fichier CSV",
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          
          tags$hr(),
          
          # Radio bouton pour choisir le type de prédiction
          radioButtons("graph_type", "Type de graphique:",
                       choices = list("Prédictions par heure" = "hourly",
                                      "Prédictions par jour" = "daily")),
          
          # Sélecteur de date et plage de dates dynamiques
          uiOutput("date_selector"),  # Sélecteur de date
          uiOutput("date_range_selector"),  # Sélecteur de plage de dates
          
          # Bouton pour lancer la prédiction
          actionButton("predict", "Faire la prédiction")
        ),
        mainPanel(
          # Output du graphique
          plotlyOutput("plotly_predictions")
        )
      )
    ),
    
    tabPanel("Rencontrez notre équipe",
             
             fluidRow(
               column(4,  
                      div(
                        class = "card",
                        style = "padding: 25px; text-align: center;",
                        h3("Timéo Baudat"),
                        img(src = "tim.jpeg", height = "200px", width = "200px", style = "border-radius: 50%;"),
                        p(""),
                        a(href = "https://www.linkedin.com/in/timeo-baudat/", "Profil LinkedIn de Timéo", target = "_blank")
                      )
               ),
               column(4,  
                      div(
                        class = "card",
                        style = "padding: 25px; text-align: center;",
                        h3("Élise Lonchampt"),
                        img(src = "élise.jpeg", height = "200px", width = "200px", style = "border-radius: 50%;"),
                        p(""),
                        a(href = "https://www.linkedin.com/in/%C3%A9lise-lonchampt-232705235/", "Profil LinkedIn d'Élise", target = "_blank")
                      )
               ),
               column(4,  
                      div(
                        class = "card",
                        style = "padding: 25px; text-align: center;",
                        h3("Marine Lebreton"),
                        img(src = "marine.jpeg", height = "200px", width = "200px", style = "border-radius: 50%;"),
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