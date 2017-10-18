library(shiny)
library(cadastertools)
library(assertthat)
library(tidyverse)
library(leaflet)

shinyServer(function(input, output, session) {
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
  })

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
  
  output$find_button <- renderUI({
    if (nrow(addresses_results()) > 0) {
      actionButton("select", label = "Find my cadaster !")
    }
  
  })
  output$map <- renderLeaflet({
    leaflet(coordinates()[[2]], options = leafletOptions(maxZoom = 18)) %>% 
      addTiles() %>% 
      addPolygons(color = "#444444") 
  })
})
