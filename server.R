library(shiny)
library(cadastertools)
library(assertthat)
library(tidyverse)
library(leaflet)

shinyServer(function(input, output, session) {
  
  #résultats de la rechrche 
  addresses_results <- eventReactive(input$search, {
    validate(
      need(nchar(input$search_address) > 10, "Not enough characters")
    )
    cadastertools::get_adress_data(input$search_address)
  })
  
  coordinates <- eventReactive(input$select, {
    validate(
      need(!is.null(input$select_addresses), "You must select an address")
    )
    selected_address <- addresses_results() %>% filter(id == input$select_addresses)
    polygons <- cadastertools::get_cadaster_sp(selected_address$citycode)
    zone <- cadastertools::select_polygone(
      polygons,
      selected_address$longitude,
      selected_address$latitude
    )
    
    list(zone, c(selected_address$longitude, selected_address$latitude))
  })
  

  
 #bouton de choix de l'adresse 
  output$choices <- renderUI({
    if (nrow(addresses_results()) == 0) {
      textOutput("No results")
    } else {
      list_of_address <- addresses_results() %>% 
        arrange(desc(score)) %>% 
        (function(dataset) {
          lapply(
            split(dataset, dataset$label), 
            FUN = function(item) { return(item$id) }
          )
        })
      selectInput("select_addresses", label = "Choose among :", choices = list_of_address)
    }
  })
  
  # bouton de recherche 
  output$find_button <- renderUI({
    if (nrow(addresses_results()) > 0) {
      actionButton("select", label = "Find my cadaster !")
    }
  
  })
  

  #gestion de la carte 
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView( 2.3037,46.4317, zoom = 6)
  })
  
  observe({
    leafletProxy("map", data = coordinates()[[1]][[2]]) %>% 
      addPolygons(color = "#444444") %>% 
      setView(coordinates()[[2]][1], coordinates()[[2]][2], zoom = 18)
  })
  
  
  get_complementary_data <- function (long, lat){
    #gets the complementary informations : 
    #parcelle number and area 
    #section of the town 
    return("lorem ipsum")
  }
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      content <- get_complementary_data(event$long, event$lat)
      leafletProxy("map") %>% addPopups(event$lng, event$lat, content)
    })
  })
  
})
