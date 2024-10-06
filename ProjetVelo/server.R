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
  
  #Création d'une variable avec la date et heure dans le bon format
  df <- read.table('SeoulBikeData.csv', sep = ",", dec=".", header=TRUE, stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1")
  df$Date <- dmy(df$Date)
  df$Hour <- paste0(sprintf("%02d", df$Hour), ":00")
  df$DateHourtemp <- paste(df$Date, df$Hour)
  df$DateHour <- ymd_hm(df$DateHourtemp)
  df$Jour <- day(df$Date)
  df$Mois <- month(df$Date)
  df$Année <- year(df$Date)
  str(df)
  
  colnames(df) <- c("Date", "Rented.Bike.Count", "Heure", "Température",
                    "Humidité", "Vitesse du vent", "Visibilité", 
                    "Température du point de rosée",
                    "Rayonnement solaire", "Précipitations", "Chutes de neige", "Saisons",
                    "Vacances", "Jour de fonctionnement", "DateHourTemp", "DateHour", "Jour",
                    "Mois","Année")
  
  df$Heure <- as.factor(df$Heure)
  df$Jour <- as.factor(df$Jour)
  df$Mois <- as.factor(df$Mois)
  df$Saisons <- as.factor(df$Saisons)
  df$Vacances <- as.factor(df$Vacances)
  df$`Jour de fonctionnement` <- as.factor(df$`Jour de fonctionnement`)
  
  quant_vars <- c("Température", "Humidité", "Vitesse du vent", "Visibilité",
                  "Température du point de rosée", "Rayonnement solaire", 
                  "Précipitations", "Chutes de neige")
  qual_vars <- c("Heure", "Jour", "Mois", "Saisons", "Vacances", "Jour de fonctionnement")
  
  # Encapsulation des données filtrées dans une fonction réactive
  filtered_data <- reactive({
    req(input$dates)  
    
    start_date <- as.POSIXct(input$dates[1])
    end_date <- as.POSIXct(input$dates[2])
    
    # Filtrer les données en fonction de la plage de dates sélectionnée
    subset(df, DateHour >= start_date & DateHour <= end_date)
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
                  name = input$varSelect, 
                  yaxis = "y2",
                  line = list(color='green'))  %>% 
        layout(title = "Quantité de vélos loués dans la ville de Séoul",
               yaxis = list(title = "Vélos loués"),
               yaxis2 = list(title = input$varSelect, overlaying = "y", side = "right"),
               xaxis = list(title = "Date et Heure"),
               hovermode = "closest")  # Définir le mode de survol
      
    } else if (input$varSelect %in% qual_vars) {
      plot_ly(dta, 
              x = ~dta[[input$varSelect]], 
              y = ~Rented.Bike.Count, 
              type = 'box') %>%
        layout(title = "Quantité de vélos loués dans la ville de Séoul",
               xaxis = list(title = input$varSelect, overlaying = "y", side = "right"),
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
                   "<strong>La variable ", input$varSelect, " a un impact significatif sur le nombre de vélos loués.</strong>",
                   '</div>'))
      } else {
        HTML(paste("<strong>La variable", input$varSelect, "n'a pas d'impact significatif sur le nombre de vélos loués.</strong>"))}
    }, error = function(e){
      HTML("<strong>Impossible de tester la significativité de la variable sélectionnée.</strong>")})
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
  
  
  # 2) ONGLET PREDICTION 
  
  # Lecture du fichier importé
  new_data <- reactive({
    req(input$file1)
    newdata <- read.csv(input$file1$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    
    # Vérification des colonnes attendues
    expected_columns <- c("Date", "Heure", "Température", "Humidité", "Vitesse.du.vent", 
                          "Visibilité", "Température.du.point.de.rosée", "Rayonnement.solaire", 
                          "Précipitations", "Chutes.de.neige", "Saisons", "Vacances", 
                          "Jour.de.fonctionnement", "Jour.de.la.semaine", "Mois")
    
    if (!all(expected_columns %in% names(newdata))) {
      stop("Le fichier CSV ne contient pas les colonnes nécessaires.")
    }
    
    # Convertir les colonnes en facteurs
    newdata$Date <- as.Date(newdata$Date, format = "%Y-%m-%d")
    newdata$Heure <- as.factor(newdata$Heure)  # Reste en tant que facteur
    newdata$Jour.de.la.semaine <- as.factor(newdata$Jour.de.la.semaine)
    newdata$Mois <- as.factor(newdata$Mois)
    
    return(newdata)
  })
  
  #Sélecteur de date pour les prédictions par heure
  output$date_selector <- renderUI({
    req(new_data())
    if (input$graph_type == "hourly") {
      selectInput("selected_date", "Sélectionnez une date:",
                  choices = unique(new_data()$Date))  # Remplir avec les dates uniques
    }
  })
  
  # Créer le sélecteur de plage de dates pour les prédictions par jour
  output$date_range_selector <- renderUI({
    req(new_data())
    if (input$graph_type == "daily") {
      dateRangeInput("date_range", "Sélectionnez une plage de dates:",
                     start = min(new_data()$Date), 
                     end = max(new_data()$Date),
                     min = min(new_data()$Date), 
                     max = max(new_data()$Date))
    }
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
    
    # Exclure la colonne "Date" avant de faire la prédiction
    new_data_predict <- new_data()[, !(names(new_data()) %in% "Date")]
    
    # Prédiction sur les nouvelles données
    predictions <- predict(modglm, newdata = new_data_predict, type = "response")
    
    # Ajouter les prédictions au jeu de données importé
    df_predictions <- new_data()
    df_predictions$Predicted <- predictions
    
    if (input$graph_type == "hourly") {
      
      # Filtrer les données selon la date sélectionnée par l'utilisateur
      df_filtered <- df_predictions[df_predictions$Date == input$selected_date,]
      
      # Vérifier si df_filtered n'est pas vide
      if (nrow(df_filtered) == 0) {
        stop("Aucune donnée trouvée pour la date sélectionnée.")
      }
      
      # Récupérer le jour de la semaine pour la date sélectionnée
      day_of_week <- unique(df_filtered$Jour.de.la.semaine)  # Utiliser unique pour éviter les doublons
      
      # Vérifier que le jour de la semaine existe
      if (length(day_of_week) == 0) {
        stop("Aucune valeur pour le jour de la semaine.")
      }
      
      # Mettre la langue en français
      Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
      
      # Formatage de la date sélectionnée pour l'affichage dans le titre
      formatted_date <- format(as.Date(input$selected_date), "%d %B %Y")
      
      # Dictionnaire pour les jours de la semaine
      jours_semaine <- c("1" = "lundi", "2" = "mardi", "3" = "mercredi", 
                         "4" = "jeudi", "5" = "vendredi", "6" = "samedi", 
                         "7" = "dimanche")
      
      # Obtenir le nom du jour de la semaine
      day_name <- jours_semaine[as.character(day_of_week)]
      
      # Vérification supplémentaire pour s'assurer que le nom du jour est correctement récupéré
      if (is.na(day_name) || length(day_name) == 0) {
        stop("Erreur dans la récupération du jour de la semaine.")
      }
      
      output$plotly_predictions <- renderPlotly({
        plot_ly(data = df_filtered, 
                x = ~Heure, 
                y = ~Predicted, 
                type = 'scatter', 
                mode = 'lines+markers', 
                name = 'Prédiction',
                hoverinfo = 'text',  # Spécifie que l'info de survol doit être définie manuellement
                text = ~paste("Heure:", sprintf("%02d:00", as.numeric(as.character(Heure))),
                              "<br>Prédiction:", round(Predicted, 2), "vélos")
        ) %>%
          layout(title = paste("Prédictions du nombre de vélos loués pour le", day_name, formatted_date),
                 xaxis = list(title = "Heure de la journée"),
                 yaxis = list(title = "Nombre de vélos loués"))
      })
    } else if (input$graph_type == "daily") {
      # Filtrer les données selon la plage de dates sélectionnée par l'utilisateur pour les prédictions par jour
      start_date <- input$date_range[1]
      end_date <- input$date_range[2]
      df_filtered <- df_predictions[df_predictions$Date >= start_date & df_predictions$Date <= end_date,]
      
      # Agréger les prédictions par jour
      df_daily <- aggregate(Predicted ~ Date, data = df_filtered, FUN = sum)
      
      output$plotly_predictions <- renderPlotly({
        plot_ly(data = df_daily, 
                x = ~Date, 
                y = ~Predicted, 
                type = 'scatter', 
                mode = 'lines+markers', 
                name = 'Prédiction',
                hoverinfo = 'text', 
                text = ~paste("Date:", format(Date, "%d %B %Y"),
                              "<br>Prédiction:", round(Predicted, 2), "vélos")
        ) %>%
          layout(title = paste("Prédictions du nombre de vélos loués du", format(start_date, "%d %B %Y"), "au", format(end_date, "%d %B %Y")),
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Nombre de vélos loués"))
      })
      
      
    }
    
  })
  
  
}