library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  titlePanel("Cadaster"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("search_address", label = "Address :", placeholder = "Enter an address", value = "6 rue du débarcadère"),
      actionButton("search", label = "Search !"),
      uiOutput("choices"),
      uiOutput("find_button")
    ),
    
    mainPanel(
      leafletOutput("map")
    ),
  )
))