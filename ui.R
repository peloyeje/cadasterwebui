library(shiny)
library(leaflet)

shinyUI(
  fluidPage(
    tags$head(includeCSS("styles.css")),
  
    # Absolute wrapper for the map (100% height)
    leafletOutput("map", width = "100%"),
  
    # Side block with inputs
    absolutePanel(
      id = "adress_input", 
      class = "panel panel-default", 
      draggable = TRUE, 
      
      h3("Veuillez rentrer votre adresse"),
      textInput("search_address", label = "", placeholder = "Enter an address", value = "6 rue du débarcadère"),
      actionButton("search", label = "Rechercher"),
      uiOutput("choices"),
      checkboxInput("draw", label = "Activer le mode calcul d'aire")
  )
))