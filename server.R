library(shiny)
library(cadastertools)
library(assertthat)
library(dplyr)
library(leaflet)

shinyServer(function(input, output, session) {
  # Base variables
  layers <- c("batiments", "parcelles", "feuilles", "sections")

  # Reactive values
  # --
  # When the search address form is submitted, search the input against the BANO database
  address_choices <- eventReactive(input$search, {
    # Check if the address is long enough
    validate(
      need(nchar(input$search_address) > 10, "Not enough characters")
    )
    
    tryCatch(
      as.data.frame(cadastertools::get_bano_matches(input$search_address)),
      error = function(e) {
        message("Error during address search :")
        message(e)
        return(NULL)
      }
    )
  })
  
  # --
  # Get address selected from the dropdown list
  selected_address <- eventReactive(input$select, {
    validate(
      need(!is.null(input$selected_address), "Merci de sélectionner une adresse")
    )

    address_choices() %>% filter(id == input$selected_address)
  })
  
  # Download all required files for the selected address
  layer_paths <- reactive({
    sapply(layers, USE.NAMES = TRUE, simplify = FALSE, FUN = function(layer) {
      cadastertools::download_cadaster(selected_address()$citycode, layer)
    })
  })

  nearest_building <- reactive({
    if (length(layer_paths()) != 0 & "batiments" %in% names(layer_paths())) {
      geo_data <- cadastertools::json_to_sf(layer_paths()$batiments)
      cadastertools::get_nearest_polygon(
        geo_data,
        selected_address()$longitude,
        selected_address()$latitude
      )
    }
  })
  
  # # Displays the addresses found on BANO
  output$choices <- renderUI({
    if (!is.null(address_choices())) {
      # Creates a new UI element and reformats the address dataset into a id/label list
      data <- address_choices() %>%
        arrange(desc(score)) %>%
        (function(dataset) {
          lapply(
            split(dataset, dataset$label),
            FUN = function(item) { return(item$id) }
          )
        })

      tagList(
        selectInput(
          "selected_address",
          label = "Choix :",
          choices = data
        ),
        actionButton(
          "select",
          label = "Trouver mon cadastre !"
        )
      )
    } else {
      textOutput("Pas de résultats")
    }
  })

  # Create the initial map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(2.3037, 46.4317, zoom = 6)
  })

  # Update the map each time nearest geo entry is updated
  observe({
    leafletProxy("map", data = nearest_building()$geometry) %>%
      clearShapes() %>% 
      clearPopups() %>% 
      addPolygons(color = "#444444", popup = formatted_complementary_data()) %>%
      setView(selected_address()$longitude, selected_address()$latitude, zoom = 18)
  })

  complementary_data <- reactive({
    sapply(layer_paths()[-1], USE.NAMES = TRUE, simplify = FALSE, FUN = function(layer) {
      df <- cadastertools::json_to_sf(layer)
      return(
        cadastertools::get_parent_polygon(nearest_building(), df)
      )
    })
  })
  
  formatted_complementary_data <- reactive({
    glue::glue("<strong>parcelle number </strong>: {parcelle_number} with area {parcelle_area} </br>
        <strong>feuille id</strong> : {feuille_id} <br>
        <strong>section id </strong>: {section_id}",
        parcelle_number = complementary_data()$parcelles$id,
        parcelle_area = cadastertools::get_area(complementary_data()$parcelles$geometry) %>% format(nsmall = 1),
        feuille_id = complementary_data()$feuilles$id,
        section_id = complementary_data()$sections$id
    )
  })
})
