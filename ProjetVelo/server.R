#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
#library(dygraphs)
library(xts)
library(lubridate)
library(factoextra)
#library(htmlwidgets)
library(plotly)
library(tidyverse)



# Define server logic required to draw a histogram
function(input, output, session) {
  
  
  #TIM et ELISE
  
  # Modification du jeu de données
  data_reactive <- reactive({
    df <- read.table('SeoulBikeData.csv', sep = ",", dec=".", header=TRUE, stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1")
    colnames(df) <- c("Date", "Rented.Bike.Count", "Heure", "Température", "Humidité", "Vitesse.du.vent", 
                      "Visibilité", "Température.du.point.de.rosée", "Rayonnement.solaire", 
                      "Précipitations", "Chutes.de.neige", "Saisons", "Vacances", 
                      "Jour.de.fonctionnement")
    df$Date <- dmy(df$Date)
    df$Hour <- paste0(sprintf("%02d", df$Heure), ":00")
    df$DateHourtemp <- paste(df$Date, df$Hour)
    df$DateHour <- ymd_hm(df$DateHourtemp)
    df$Jour <- day(df$Date)
    df$Mois <- month(df$Date)
    df$Année <- year(df$Date)
    df$Jour.de.la.semaine <- wday(df$Date, label = TRUE, abbr = FALSE, week_start = 1, locale = "fr_FR")
    df$Saisons <- as.character(df$Saisons)
    df$Jour.de.fonctionnement <- as.character(df$Jour.de.fonctionnement)
    df$Vacances <- as.character(df$Vacances)
    df <- df %>%
      mutate(
        Saisons = recode(Saisons,
                         `Spring` = "Printemps",
                         `Summer` = "Été",
                         `Autumn` = "Automne",
                         `Winter` = "Hiver"),
        Jour.de.fonctionnement = recode(Jour.de.fonctionnement,
                                        `Yes` = "Oui",
                                        `No` = "Non"),
        Vacances = recode(Vacances,
                          `No Holiday` = "Non",
                          `Holiday` = "Oui"))
  
    df$Heure <- as.factor(df$Heure)
    df$Jour <- as.factor(df$Jour)
    df$Mois <- as.factor(df$Mois)
    df$Saisons <- as.factor(df$Saisons)
    df$Vacances <- as.factor(df$Vacances)
    df$Jour.de.fonctionnement <- as.factor(df$Jour.de.fonctionnement)
    df$Jour.de.la.semaine <- as.factor((df$Jour.de.la.semaine))
    
    str(df)
    
    return(df)
  })
  
  quant_vars <- c("Température", "Humidité", "Vitesse.du.vent", "Visibilité", 
                    "Température.du.point.de.rosée", "Rayonnement.solaire", 
                    "Précipitations", "Chutes.de.neige")
  qual_vars <- c("Heure", "Jour", "Mois", "Saisons", "Vacances", "Jour.de.fonctionnement", "Jour.de.la.semaine")
  
  labels <- c("Température" = "Température (°C)",
              "Humidité" = "Humidité (%)",
              "Vitesse.du.vent" = "Vitesse du vent (m/s)",
              "Visibilité" = "Visibilité (m)",
              "Température.du.point.de.rosée" = "Température du point de rosée (°C)",
              "Rayonnement.solaire" = "Rayonnement solaire (MJ/m2)",
              "Précipitations" = "Précipitations (mm)",
              "Chutes.de.neige" = "Chutes de neige (cm)",
              "Heure" = "Heure", 
              "Jour" = "Jour",
              "Mois" = "Mois",
              "Saisons" = "Saisons", 
              "Vacances" = "Vacances",
              "Jour.de.fonctionnement" = "Jour de fonctionnement",
              "Jour.de.la.semaine" = "Jour de la semaine")
  
  # Encapsulation des données filtrées dans une fonction réactive
  filtered_data <- reactive({
    req(input$dates)  
    
    start_date <- as.POSIXct(input$dates[1])
    end_date <- as.POSIXct(input$dates[2])
    
    # Récupération de df
    df <- data_reactive()
    
    # Filtrage des données en fonction de la plage de dates sélectionnée
    df <- subset(df, DateHour >= start_date & DateHour <= end_date)
    
    # Filtrage supplémentaire si choix de la variable "Heure"
    if (input$varSelect == "Heure"){
      if (input$typeJour == "Week-end"){
        df <- df %>% filter(Jour.de.la.semaine %in% c("Samedi", "Dimanche"))}
      else if (input$typeJour == "Semaine"){
        df <- df %>% filter(Jour.de.la.semaine %in% c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi"))}
    }
    return(df)
  })
  
  output$varSelectUI <- renderUI({
    df <- data_reactive()  # Appel du jeu de données réactif
    
    selectInput("varSelect", 
                "Choisir la variable à analyser :",
                choices = setNames(names(df)[-c(1,2,15,16,17,18,20)], 
                                   gsub("\\.", " ", names(df)[-c(1,2,15,16,17,18,20)])))
  })
  
  output$linePlot <- renderPlotly({
    dta <- filtered_data()
    
    if (input$varSelect %in% quant_vars) {
      
      plot_ly(dta, 
              x = ~DateHour, 
              y = ~Rented.Bike.Count, 
              type = 'scatter', 
              mode = 'lines',
              text = ~paste("Date:", DateHour, "<br>Valeur:", Rented.Bike.Count),
              name = "Nombre de vélos loués",
              hoverinfo = 'text') %>%
        # Ajouter la deuxième variable sélectionnée par l'utilisateur
        add_trace(y = ~dta[[input$varSelect]],  
                  type = 'scatter', 
                  mode = 'lines', 
                  text = ~paste("Date:", DateHour, "<br>Valeur:", dta[[input$varSelect]]),
                  hoverinfo = 'text', 
                  name = labels[input$varSelect], 
                  yaxis = "y2",
                  line = list(color='green'))  %>% 
        layout(title = "Quantité de vélos loués dans la ville de Séoul",
               yaxis = list(title = "Vélos loués"),
               yaxis2 = list(title = labels[input$varSelect], overlaying = "y", side = "right"),
               xaxis = list(title = "Date et Heure"),
               hovermode = "closest")  # Définir le mode de survol
      
    } else if (input$varSelect %in% qual_vars) {
      plot_ly(dta, 
              x = ~dta[[input$varSelect]], 
              y = ~Rented.Bike.Count, 
              type = 'box') %>%
        layout(title = "Quantité de vélos loués dans la ville de Séoul",
               xaxis = list(title = labels[input$varSelect], overlaying = "y", side = "right"),
               yaxis = list(title = "Vélos loués"))
    } 
  })
  
  # Info sur variable
  output$anova_result <- renderUI({
    dta <- filtered_data()
    result_text <- ""
    
    ## ANOVA
    tryCatch({
      aov_model <- aov(Rented.Bike.Count ~ dta[[input$varSelect]], data = dta)
      anova_summary <- summary(aov_model)
      p_value <- anova_summary[[1]][["Pr(>F)"]][1]
      if (p_value <= 0.05) {
        HTML(paste('<div style="border: 2px solid #660000; padding: 10px; border-radius: 5px;">',
                   "<strong>La variable ", labels[input$varSelect], " a un impact significatif sur le nombre de vélos loués.</strong>",
                   '</div>'))
      } else {
        HTML(paste('<div style="border: 2px solid #660000; padding: 10px; border-radius: 5px;">',
                   "<strong>La variable", labels[input$varSelect], "n'a pas d'impact significatif sur le nombre de vélos loués.</strong>",
                   '</div>'))}
    }, error = function(e){
      HTML('<div style="border: 2px solid #660000; padding: 10px; border-radius: 5px;">',
           "<strong>Impossible de tester la significativité de la variable sélectionnée.</strong>",
           '</div>')})
  })
  
  # Logique pour afficher la blague lorsque le bouton est cliqué
  output$joke_text <- renderText({
    req(input$joke_button)  # Nécessite que le bouton soit cliqué
    "Pourquoi les vélos sont-ils toujours fatigués? Parce qu'ils ont des pneus crevés!"
  })
  #MARINE
  
  #1) ONGLET STRUCTURE DU JEU DE DONNEES
  
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
      "Température" = "Température en °C",
      "Humidité" = "Humidité en %",
      "Vitesse.du.vent" = "Vitesse du vent en m/s",
      "Visibilité" = "Visibilité en 10m",
      "Température.du.point.de.rosée" = "Température du point de rosée en °C",
      "Rayonnement.solaire" = "Rayonnement solaire en MJ/m2",
      "Précipitations" = "Précipitation en mm",
      "Chutes.de.neige" = "Chutes de neige en cm"
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
    df <- data_reactive()
    DT::datatable(
      df[,-(15:17)],
      options = list(scrollX = TRUE)  # Activer le défilement horizontal
    )
  })
  
  # 2) ONGLET PREDICTION 
  
  # Lecture du fichier importé
  new_data <- reactive({
    req(input$file1)
    newdata <- read.csv(input$file1$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    
    # Vérification des colonnes attendues
    expected_columns <- c("Heure", "Température", "Humidité", "Vitesse.du.vent", 
                          "Visibilité", "Température.du.point.de.rosée", "Rayonnement.solaire", 
                          "Précipitations", "Chutes.de.neige", "Saisons", "Vacances", 
                          "Jour.de.fonctionnement", "Jour.de.la.semaine", "Mois")
    
    if(!all(expected_columns %in% names(newdata))) {
      stop("Le fichier CSV ne contient pas les colonnes nécessaires.")
    }
    
    # Convertir les colonnes en facteurs
    newdata$Heure <- as.factor(newdata$Heure)  # Reste en tant que facteur
    newdata$Jour.de.la.semaine <- as.factor(newdata$Jour.de.la.semaine)
    newdata$Mois <- as.factor(newdata$Mois)
    
    return(newdata)
  })
  
  df_mod <- read.table('SeoulBikeData.csv', sep = ",", dec=".", header=TRUE, stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1")
  df_mod$Date <- dmy(df_mod$Date)
  df_mod$Hour <- as.factor(df_mod$Hour)  # Gardez comme facteur
  df_mod$Jour.de.la.semaine <- as.factor(format(df_mod$Date, "%u"))
  df_mod$Mois <- as.factor(month(df_mod$Date))
  
  df_mod <- df_mod[,2:16]
  
  colnames(df_mod) <- c("Rented.Bike.Count", "Heure", "Température",
                        "Humidité", "Vitesse.du.vent", "Visibilité", 
                        "Température.du.point.de.rosée",
                        "Rayonnement.solaire", "Précipitations", "Chutes.de.neige", "Saisons",
                        "Vacances", "Jour.de.fonctionnement", "Jour.de.la.semaine",
                        "Mois")
  
  # Faire la prédiction lorsque l'utilisateur appuie sur le bouton
  observeEvent(input$predict, {
    req(new_data())  # S'assurer que les données sont chargées
    
    # Modèle que vous avez déjà créé (utilisez votre modèle ici)
    modglm <- glm(Rented.Bike.Count ~ ., family = 'poisson', data = df_mod)  # Remplacez df_mod par votre jeu de données
    
    # Prédiction sur les nouvelles données
    predictions <- predict(modglm, newdata = new_data(), type = "response")
    
    # Ajouter les prédictions au jeu de données importé
    df_predictions <- new_data()
    df_predictions$Predicted <- predictions
    
    # Créer la variable DateHour à partir de Heure (en supposant que vous avez un format temporel fixe)
    # Par exemple, en utilisant une base fixe pour la date (ex: le 1er janvier 2024)
    base_date <- "2024-01-01"  # Assurez-vous que cette date convient à votre contexte
    df_predictions$DateHour <- as.POSIXct(paste(base_date, sprintf("%02d:00:00", as.numeric(as.character(df_predictions$Heure)))), tz = "UTC")
    
    # Vérifiez les longueurs
    if (length(df_predictions$Predicted) != length(df_predictions$DateHour)) {
      stop("Les longueurs des prédictions et des DateHour ne correspondent pas.")
    }
    
    # Convertir les données en xts pour dygraphs
    df_xts <- xts(df_predictions$Predicted, order.by = df_predictions$DateHour)
    
    # Afficher les prédictions dans un graphique interactif
    output$dygraph_predictions <- renderDygraph({
      dygraph(df_xts, main = "Prédiction du nombre de vélos loués par heure") %>%
        dyAxis("y", label = "Nombre de vélos loués") %>%
        dyAxis("x", label = "Heure de la journée")
    })
  })
  
  

  
}