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
  
  output$date_message <- renderText({
    # Vérifier si la date de début est supérieure à la date de fin
    if (!is.null(input$dates)) {
      dates <- input$dates
      start_date <- dates[1]
      end_date <- dates[2]
      
      if (start_date > end_date) {
        return(HTML("<div style='color: red; font-weight: bold; margin-bottom: 10px;'>
                      La date de début doit être antérieure à la date de fin.
                    </div>"))
      } 
    }
  })
  
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
                  line = list(color='#1ABC9C'))  %>% 
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
  
  # ANOVA
  output$anova_result <- renderUI({
    dta <- filtered_data()

    tryCatch({
      aov_model <- aov(Rented.Bike.Count ~ dta[[input$varSelect]], data = dta)
      anova_summary <- summary(aov_model)
      p_value <- anova_summary[[1]][["Pr(>F)"]][1]
      if (p_value <= 0.05) {
        HTML(paste('<div style="border: 2px solid #1ABC9C; padding: 10px; border-radius: 5px;">',
                   "La variable ", labels[input$varSelect], " a un <strong> impact significatif </strong> sur le nombre de vélos loués.",
                   '</div>'))
      } else {
        HTML(paste('<div style="border: 2px solid #1ABC9C; padding: 10px; border-radius: 5px;">',
                   "La variable", labels[input$varSelect], "n'a <strong> pas d'impact significatif </strong> sur le nombre de vélos loués.",
                   '</div>'))}
    }, error = function(e){
      HTML('<div style="border: 2px solid #1ABC9C; padding: 10px; border-radius: 5px;">',
           "Impossible de tester la significativité de la variable sélectionnée.",
           '</div>')})
  })
  

  blague <- c("Pourquoi les vélos sont-ils toujours fatigués? Parce qu'ils ont des pneus crevés!",
  "Quel est le comble pour un cycliste? De perdre les pédales !")

  ## Coefficient de corrélation
  output$correlation <- renderUI({
    dta <- filtered_data()
    tryCatch({
      if (input$varSelect %in% quant_vars){
        correlation_value <- cor(dta$Rented.Bike.Count, dta[[input$varSelect]], use = "complete.obs")
        if (is.na(correlation_value)){
          HTML('<div style="border: 2px solid #1ABC9C; padding: 10px; border-radius: 5px;">',
               "Impossible de calculer le coefficient de corrélation des variables sélectionnées.",
               '</div>')
        }else{
          HTML(paste('<div style="border: 2px solid #1ABC9C; padding: 10px; border-radius: 5px;">',
                     "Le coefficient de corrélation entre",labels[input$varSelect],"et le nombre de vélos loués est :", '<strong>',sprintf("%.2f", correlation_value), '</strong>',
                     '</div>'))
        }
      }
    }, error = function(e){
      HTML('<div style="border: 2px solid #1ABC9C; padding: 10px; border-radius: 5px;">',
           "Impossible de calculer le coefficient de corrélation des variables sélectionnées.",
           '</div>')})
   
  })
    
      
  # Logique pour afficher la blague lorsque le bouton est cliqué
  output$joke_text <- renderText({
    req(input$joke_button)  # Nécessite que le bouton soit cliqué
    sample(blague,1)
  })
  #MARINE
  
  #1) ONGLET STRUCTURE DU JEU DE DONNEES
  
  # Calcul du nombre de lignes
  output$nb_lignes <- renderText({
    df <- data_reactive()
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
      "Date" = "Date (jour, mois et année)",
      "Heure" = "Heure de la journée",
      "Saisons" = "Saison : hiver, printemps, été ou automne",
      "Vacances" = "Vacances : oui ou non",
      "Jour.de.fonctionnement" = "Jour de fonctionnement : oui ou non"
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
  
  output$download_model <- downloadHandler(
    filename = function() {
      "modele_bike_prediction.csv"
    },
    content = function(file) {
      # Nombre de lignes, ici basé sur 24 heures
      n <- 24
      
      # Créer un modèle de fichier CSV avec les colonnes attendues
      model_data <- data.frame(
        Date = rep(Sys.Date(), n),  # Répéter la date pour correspondre aux heures
        Heure = 0:23,  # Exemple pour les heures de la journée
        Température = rep(NA, n),
        Humidité = rep(NA, n),
        Vitesse.du.vent = rep(NA, n),
        Visibilité = rep(NA, n),
        Température.du.point.de.rosée = rep(NA, n),
        Rayonnement.solaire = rep(NA, n),
        Précipitations = rep(NA, n),
        Chutes.de.neige = rep(NA, n),
        Saisons = rep(factor(c("Printemps", "Été", "Automne", "Hiver")), length.out = n),  # Répéter pour chaque ligne
        Vacances = rep(factor(c("Oui", "Non")), length.out = n),  # Répéter pour chaque ligne
        Jour.de.fonctionnement = rep(factor(c("Oui", "Non")), length.out = n)  # Répéter pour chaque ligne
      )
      
      # Écrire le fichier CSV modèle
      write.csv(model_data, file, row.names = FALSE)
    }
  )
  
  # Lecture du fichier importé
  new_data <- reactive({
    req(input$file1)
    newdata <- read.csv(input$file1$datapath, header = TRUE, sep = ";", stringsAsFactors = FALSE)
    
    # Vérification des colonnes attendues
    expected_columns <- c("Date", "Heure", "Température", "Humidité", "Vitesse.du.vent", 
                          "Visibilité", "Température.du.point.de.rosée", "Rayonnement.solaire", 
                          "Précipitations", "Chutes.de.neige", "Saisons", "Vacances", 
                          "Jour.de.fonctionnement")
    
    if (!all(expected_columns %in% names(newdata))) {
      stop("Le fichier CSV ne contient pas les colonnes nécessaires.")
    }
    
    # Convertir les colonnes en facteurs
    newdata$Date <- as.Date(newdata$Date, format = "%Y-%m-%d")
    newdata$Heure <- as.factor(newdata$Heure)  # Reste en tant que facteur
    newdata$Jour.de.la.semaine <- wday(newdata$Date, label = TRUE, abbr = FALSE, week_start = 1, locale = "fr_FR")
    newdata$Mois <- month(newdata$Date)
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
  colnames(df_mod) <- c("Date", "Rented.Bike.Count", "Heure", "Température", "Humidité", "Vitesse.du.vent", 
                        "Visibilité", "Température.du.point.de.rosée", "Rayonnement.solaire", 
                        "Précipitations", "Chutes.de.neige", "Saisons", "Vacances", 
                        "Jour.de.fonctionnement")
  df_mod$Date <- dmy(df_mod$Date)
  df_mod$Mois <- month(df_mod$Date)
  df_mod$Jour.de.la.semaine <- wday(df_mod$Date, label = TRUE, abbr = FALSE, week_start = 1, locale = "fr_FR")
  df_mod$Saisons <- as.character(df_mod$Saisons)
  df_mod$Jour.de.fonctionnement <- as.character(df_mod$Jour.de.fonctionnement)
  df_mod$Vacances <- as.character(df_mod$Vacances)
  df_mod <- df_mod %>%
    mutate(
      Saisons = case_when(
        Saisons == "Spring" ~ "Printemps",
        Saisons == "Summer" ~ "Été",
        Saisons == "Autumn" ~ "Automne",
        Saisons == "Winter" ~ "Hiver",
        TRUE ~ Saisons  # Cette ligne est importante pour conserver les autres valeurs non modifiées
      ),
      Jour.de.fonctionnement = case_when(
        Jour.de.fonctionnement == "Yes" ~ "Oui",
        Jour.de.fonctionnement == "No" ~ "Non",
        TRUE ~ Jour.de.fonctionnement
      ),
      Vacances = case_when(
        Vacances == "No Holiday" ~ "Non",
        Vacances == "Holiday" ~ "Oui",
        TRUE ~ Vacances
      )
    )
  df_mod$Heure <- as.factor(df_mod$Heure)
  df_mod$Mois <- as.factor(df_mod$Mois)
  df_mod$Saisons <- as.factor(df_mod$Saisons)
  df_mod$Vacances <- as.factor(df_mod$Vacances)
  df_mod$Jour.de.fonctionnement <- as.factor(df_mod$Jour.de.fonctionnement)
  df_mod$Jour.de.la.semaine <- as.factor((df_mod$Jour.de.la.semaine))
  
  df_mod <- df_mod[,2:16]
  
  
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
      day_name <- unique(df_filtered$Jour.de.la.semaine)  # Utiliser unique pour éviter les doublons
      
      # Mettre la langue en français
      Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
      
      # Formatage de la date sélectionnée pour l'affichage dans le titre
      formatted_date <- format(as.Date(input$selected_date), "%d %B %Y")
      
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
                              "<br>Prédiction:", round(Predicted, 0), "vélos")
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
                              "<br>Prédiction:", round(Predicted, 0), "vélos")
        ) %>%
          layout(title = paste("Prédictions du nombre de vélos loués du", format(start_date, "%d %B %Y"), "au", format(end_date, "%d %B %Y")),
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Nombre de vélos loués"))
      })
    }
    
  })
  
  
}