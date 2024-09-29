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

## Listes des fonctions

modif_outpout_var <- function(x){
  if (x == "Vitesse du vent"){x<-"Vitesse.du.vent"}
  else if (x == "Température du point de rosée"){x<-"Température.du.point.de.rosée"}
  else if (x == "Rayonnement solaire"){x<-"Rayonnement.solaire"}
  else if (x == "Chutes de neige"){x<-"Chutes.de.neige"}
  else if (x=="Jour de fonctionnement"){x<-"Jour.de.fonctionnement"}
  else {x <- x}
  return(x)
}

get_p_value <- function(x){
  x <- modif_outpout_var (x)
  mod <- Anova(lm(`Nombre de vélos loués` ~ df[[x]], data = df), type ="III")
  p_value <- mod$`Pr(>F)`[2]
  return(p_value)
}

# Define server logic required to draw a histogram
function(input, output, session) {

  
#TIM
  
  # Importation du jeu de données et pré-traitement des variables
  df <- read.table('SeoulBikeData.csv', sep = ";", dec=".", header=TRUE, stringsAsFactors = TRUE, fileEncoding = "UTF-8")
  df$Date <- dmy(df$Date)
  df$Hour <- paste0(sprintf("%02d", df$Hour), ":00")
  df$DateHourtemp <- paste(df$Date, df$Hour)
  df$DateHour <- ymd_hm(df$DateHourtemp)
  df$Date <- dmy(df$Date)
  df$Jour <- day(df$Date)
  df$Mois <- month(df$Date)
  df$Année <- year(df$Date)
  df$Jour <- as.factor(df$Jour)
  df$Mois <- as.factor(df$Mois)
  df$Année <- as.factor(df$Année)
  df$Heure <- as.factor(df$Heure)
  
  # Identification des variables quantitatives et qualitatives
  quant_vars <- c("Température", "Humidité", "Vitesse du vent", "Visibilité",
                    "Température du point de rosée", "Rayonnement solaire", 
                    "Précipitations", "Chutes de neige")
  qual_vars <- c("Heure", "Jour", "Mois", "Saisons", "Vacances", "Jour de fonctionnement")
  
  # Création des graphiques
  output$variablePlot <- renderPlot({
    if (input$varSelect %in% quant_vars){
      boxplot(`Nombre de vélos loués` ~ df[[input$varSelect]], data = df, 
              xlab = input$varSelect, 
              ylab = "Nombre de vélos loués", 
              main = paste("Distribution des vélos loués par heure"))}

    else if (input$varSelect %in% qual_vars){#TIM
      }
  })
  
  # Calcul de la p-value de l'ANOVA
  p_value <- get_p_value(input$varSelect)
  
  # Affichage de la p-value
  output$significativite <- renderUI({
    if (p_value<=0.05){
      wellPanel(
        tags$h4(style = "color: green:",
                paste ("La variable ", input$varSelect, " influence significativement le nombre de vélos loués."))
      )
    }
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

