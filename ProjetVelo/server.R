#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dygraphs)
library(xts)
library(lubridate)
library(factoextra)

# Define server logic required to draw a histogram
function(input, output, session) {

  
#TIM
  
  df <- read.table('SeoulBikeData.csv', sep = ",", dec=".", header=TRUE, stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1")
  df$Date <- dmy(df$Date)
  df$Hour <- paste0(sprintf("%02d", df$Hour), ":00")
  df$DateHourtemp <- paste(df$Date, df$Hour)
  df$DateHour <- ymd_hm(df$DateHourtemp)
  print(df)
  
  output$linePlot <- renderDygraph({
    req(input$dates)  
    
    start_date <- as.POSIXct(input$dates[1])
    end_date <- as.POSIXct(input$dates[2])
    
    # Filtrer les données en fonction de la plage de dates sélectionnée
    filtered_data <- subset(df, DateHour >= start_date & DateHour <= end_date)
    
    # Créer l'objet xts avec les données filtrées
    dy_data <- xts(filtered_data$Rented.Bike.Count, order.by = filtered_data$DateHour)
    
    # Créer le graphique dygraph
    dygraph(dy_data, main = "Graphique interactif avec dygraphs") %>%
      dyAxis("x", label = "Date") %>%
      dyAxis("y", label = "Vélos loués") %>%
      dyOptions(strokeWidth = 2)
  })
  
#ELISE
  
  # Chargement des données - Si dta_MFA est déjà chargé
  dta_MFA <- reactive({
    dta_MFA <- dta[,-1]  # Suppression de la première colonne
    return(dta_MFA)
  })
  
  # Fonction qui effectue l'AFM
  res_AFM <- eventReactive(input$run_afm, {
    # Lancement de l'AFM lorsque l'utilisateur clique sur le bouton
    dta_data <- dta_MFA()
    res <- MFA(dta_data, 
               group = c(8, 7),          # Nombre de variables dans chaque groupe
               type = c("s", "n"),        # Type des variables : "s" pour quanti, "n" pour quali
               name.group = c("Météo", "Temporalité"),
               graph = FALSE)             # Désactivation des graphiques automatiques
    return(res)
  })
  
  # Résumé des résultats de l'AFM
  output$afm_summary <- renderPrint({
    req(res_AFM())  # Attendre que l'AFM soit lancée
    summary(res_AFM())
  })
  
  # Graphique de l'AFM
  output$afm_plot <- renderPlot({
    req(res_AFM())
    fviz_mfa_var(res_AFM(), "group")  # Visualisation des groupes
  })
  
#MARINE
  
  # Calcul du nombre de lignes
  output$nb_lignes <- renderText({
    nrow(df)
  })
  
  output$var_reponse <- renderUI({
    # Nom de la variable réponse et sa description
    reponse_var <- list(
      "Rented.Bike.Count" = "Nombre de vélos loués (Variable quantitative sans unité)" 
    )
    
    # Formatage pour affichage de la variable réponse (sans gras)
    HTML(paste(sapply(names(reponse_var), function(var) {
      paste("<strong>", var, ":</strong> ", reponse_var[[var]])
    }), collapse = "<br>"))
  })
  # Variables explicatives quantitatives avec descriptions et unités directement incluses
  output$var_quant <- renderUI({
    # Liste des variables quantitatives avec leurs unités et descriptions
    quant_vars <- list(
      "Temperature..C." = "Température en °C",
      "Humidity..." = "Humidité en %",
      "Wind.speed..m.s." = "Vitesse du vent en m/s",
      "Visibility..10m." = "Visibilité en 10m",
      "Dew.point.temperature..C." = "Température du point de rosée en °C",
      "Solar.Radiation..MJ.m2." = "Rayonnement solaire en MJ/m2",
      "Rainfall.mm." = "Précipitation en mm",
      "Snowfall..cm." = "Chutes de neige en cm"
    )
    
    HTML(paste(sapply(names(quant_vars), function(var) {
      paste("<strong>", var, ":</strong> ", quant_vars[[var]])
    }), collapse = "<br>"))
  })

  # Variables explicatives qualitatives avec descriptions
  output$var_qual <- renderUI({
    # Liste des variables qualitatives avec descriptions
    qual_vars <- list(
      "Date" = "Date (jour, mois et année",
      "Hour" = "Heure de la journée",
      "Seasons" = "Saison : hiver, printemps, été ou automne",
      "Holiday" = "Vacances : oui ou non",
      "Functioning.Day" = "Jour de fonctionnement : oui ou non"
    )
    
    # Formatage des variables qualitatives
    HTML(paste(sapply(names(qual_vars), function(var) {
      paste("<strong>", var, ":</strong> ", qual_vars[[var]])
    }), collapse = "<br>"))
  })
  
  #Visualisation du jeu de données 
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      df[,-(15:16)],  # Votre jeu de données
      options = list(scrollX = TRUE)  # Activer le défilement horizontal
    )
  })
}

