library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  # titlePanel("Cadaster"),
  # 
  # sidebarLayout(
  #   sidebarPanel(
  #     textInput("search_address", label = "Address :", placeholder = "Enter an address", value = "6 rue du débarcadère"),
  #     actionButton("search", label = "Search !"),
  #     uiOutput("choices"),
  #     uiOutput("find_button")
  #   ),
  # 
  #   mainPanel(
  #     leafletOutput("map")
  #   )
  # )
  tags$head(
    includeCSS("styles.css")
  ),
  
  leafletOutput("map", width = "100%"),
  
  # Shiny versions prior to 0.11 should use class = "modal" instead.
  absolutePanel(id = "adress_input", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",

                h2("veuillez rentrer votre adresse"),

                textInput("search_address", label = "Address :", placeholder = "Enter an address", value = "6 rue du débarcadère"),
                actionButton("search", label = "Search !"),
                uiOutput("choices"),
                uiOutput("find_button")
                )
))