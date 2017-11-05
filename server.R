library(shiny)
library(cadastertools)
library(assertthat)
library(dplyr)
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
    zone <- cadastertools::get_nearest_polygon(
      polygons,
      selected_address$longitude,
      selected_address$latitude
    )
    list(zone, selected_address)
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
      selectInput("select_addresses", label = "Choix :", choices = list_of_address)
    }
  })
  
  # bouton de recherche 
  output$find_button <- renderUI({
    if (nrow(addresses_results()) > 0) {
      actionButton("select", label = "Trouver mon cadastre !")
    }
    
  })
  
  
  #Map and polygon handeling 
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView( 2.3037,46.4317, zoom = 6)
  })
  
  observe({
    leafletProxy("map") %>% clearShapes()
    leafletProxy("map", data = coordinates()[[1]]) %>% 
      addPolygons(color = "#444444") %>% 
      setView(coordinates()[[2]]$longitude, coordinates()[[2]]$latitude, zoom = 18)
  })
  
  
  #Complementary data handeling 
  
  get_complementary_data <- function (adress_data){
    #parcelle info 
    parcelle <- get_cadaster_sp(adress_data$citycode, layer = "parcelles") %>% 
      get_nearest_polygon(adress_data$longitude, adress_data$latitude)
    parcelle_number <- get_parcelle_number(parcelle)
    parcelle_area <- get_area(parcelle)
    
    #feuille info
    feuille <- get_cadaster_sp(adress_data$citycode, layer = "feuilles") %>% 
      get_nearest_polygon(adress_data$longitude, adress_data$latitude)
    feuille_id <- get_id(feuille)
    
    #section info 
    section <- get_cadaster_sp(adress_data$citycode, layer = "sections") %>% 
      get_nearest_polygon(adress_data$longitude, adress_data$latitude)
    section_id <- get_id(section)
    
    content <- glue::glue("<strong>parcelle number </strong>: {parcelle_number} with area {parcelle_area} </br>
                          <strong>feuille id</strong> : {feuille_id} <br>
                          <strong>section id </strong>: {section_id}")
    
    return(content)
  }
  
  complentary_content <- reactive({
    get_complementary_data(coordinates()[[2]])
  })
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      leafletProxy("map") %>% addPopups(event$lng, event$lat, complentary_content())
    })
  })
  
})
