library(shiny)
library(assertthat)
library(dplyr)
library(leaflet)
library(cadastertools)

shinyServer(function(input, output, session) {
  # Base variables
  layers <- c("batiments", "parcelles", "feuilles", "sections")

  # Reactive values
  # ---

  # Container from the draw mode
  draw <- reactiveValues(
    coords = list(),
    area_toggled = FALSE
  )

  # When the search address form is submitted, search the input against the BANO database
  address_choices <- eventReactive(input$search, {
    # Check if the address is long enough
    validate(
      need(nchar(input$search_address) > 10, "L'adresse est trop courte")
    )

    # Search on BANO
    tryCatch(
      as.data.frame(cadastertools::get_bano_matches(input$search_address)),
      error = function(e) {
        message("Une erreur s'est produite durant la recherche : ")
        message(e)
        return(NULL)
      }
    )
  })

  # Get selected address from the dropdown list
  selected_address <- eventReactive(input$select, {
    validate(
      need(!is.null(input$selected_address), "Merci de sélectionner une adresse")
    )

    address_choices() %>% filter(id == input$selected_address)
  })

  # Downloads all required files for the selected address and returns local file paths
  layer_paths <- reactive({
    sapply(layers, USE.NAMES = TRUE, simplify = FALSE, FUN = function(layer) {
      cadastertools::download_cadaster(selected_address()$citycode, layer)
    })
  })

  # Parse GeoJSON data and get nearest polygon from address point
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

  # Additionnal polygons
  complementary_data <- reactive({
    sapply(layer_paths()[-1], USE.NAMES = TRUE, simplify = FALSE, FUN = function(layer) {
      df <- cadastertools::json_to_sf(layer)
      return(
        cadastertools::get_parent_polygon(nearest_building(), df)
      )
    })
  })

  # Popin content with the additionnal information
  formatted_complementary_data <- reactive({
    glue::glue("<strong>parcelle number </strong>: {parcelle_number} with area {parcelle_area} </br>
               <strong>feuille id</strong> : {feuille_id} <br>
               <strong>section id </strong>: {section_id}",
               parcelle_number = complementary_data()$parcelles[[2]],
               parcelle_area = cadastertools::get_area(complementary_data()$parcelles$geometry) %>% format(ndigits = 2, nsmall = 1),
               feuille_id = complementary_data()$feuilles$id,
               section_id = complementary_data()$sections$id
    )
  })

  # Logic
  # ---

  # Displays the addresses found on BANO in the select input
  output$choices <- renderUI({
    if (!is.null(address_choices())) {
      # Reformats the address dataset into a id/label list
      data <- address_choices() %>%
        arrange(desc(score)) %>%
        (function(dataset) {
          lapply(
            split(dataset, dataset$label),
            FUN = function(item) { return(item$id) }
          )
        })

      # Create the UI element and populate it with the data
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

  # When there is a click on the map
  observeEvent(input$map_click, {
    # What follows only takes place in draw mode
    if (input$draw) {
      # Add new point to list
      point <- c(input$map_click$lng, input$map_click$lat)
      draw$coords[[length(draw$coords) + 1]] <- point

      # Create polygon and render it on the map
      draw$polygon <- cadastertools::polygon_from_list(draw$coords)
      draw$area <- cadastertools::get_area(draw$polygon) %>% format(ndigits = 2, nsmall = 1)

      leafletProxy("map", data = draw$polygon) %>%
        clearShapes() %>%
        clearPopups() %>%
        addPolygons(color = "#ff9999")
    }
  })

  # When there is a click on a polygon
  observeEvent(input$map_shape_click, {
    # If we're in draw mode
    if (input$draw) {
      # If a polygon is currently displayed
      if (!is.null(draw$polygon)) {
        # If the tooltip isn't already showed
        if (!draw$area_toggled) {
          content <- glue::glue("<strong>Area</strong> : {area}", area = draw$area)

          leafletProxy("map") %>%
            addPopups(
              input$map_shape_click$lng,
              input$map_shape_click$lat,
              content
            )

          draw$area_toggled = TRUE
        } else {
          leafletProxy("map") %>% clearPopups()
          draw$area_toggled = FALSE
        }
      }
    }
  })

  # On toggle ...
  observeEvent(input$draw, {
    # Reset values
    draw$coords = list()
    draw$polygon = NULL
    draw$area_toggled = FALSE
    # Remove any polygon from view
    leafletProxy("map") %>%
      clearShapes() %>%
      clearPopups()
  })
})
