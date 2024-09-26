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
  
}
