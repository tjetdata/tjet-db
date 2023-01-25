shinyServer(function(input, output, session) {
  highlighted <- reactive({
    country_shapes %>%
      filter(.data$country_name %in% req(input$country)) %>%
      arrange(country_name) %>%
      tibble() %>%
      select(country_name, gwcode, fid)
  })
  output$worldmap <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = FALSE,
      zoomSnap = 0.25,
      zoomDelta = 0.5,
      dragging = TRUE,
      minZoom = 2.5,
      maxZoom = 10
    )) %>%
      # setView(lng = 0, lat = 0, zoom = 2) %>%
      fitBounds(
        lng1 = 180,
        lat1 = 75,
        lng2 = -180,
        lat2 = -60
      ) %>%
      setMaxBounds(
        lng1 = 180,
        lat1 = 75,
        lng2 = -180,
        lat2 = -60
      ) %>%
      # addProviderTiles(providers$Stamen.TonerBackground, # Esri.WorldTerrain, Esri.WorldPhysical, Esri.WorldImagery, Stamen.TonerLite, Stamen.TonerBackground
      # options = providerTileOptions(minZoom = 2, maxZoom = 10, noWrap = FALSE, detectRetina = FALSE)) %>%
      # addCircleMarkers(data = capitals, weight = 3, fill = FALSE, radius = 1) %>%
      addPolygons(
        data = country_shapes,
        weight = 1.5,
        color = "darkgray",
        fillColor = "lightgray",
        fill = TRUE,
        opacity = 1,
        fillOpacity = 0.5,
        layerId = as.character(country_shapes$country_name),
        group = "base",
        label = country_shapes$country_name
      ) %>% 
      addPolygons(
        data = country_shapes,
        weight = 0,
        color = "red",
        fill = TRUE,
        opacity = 1,
        fillOpacity = 1,
        layerId = as.character(country_shapes$fid),
        group = ~ country_name,
        highlightOptions = highlightOptions(
          color = "blue",
          weight = 2,
          bringToFront = FALSE
        ),
        label = ~ country_name
      ) %>%
      hideGroup(group = country_shapes$country_name)
  })
  observeEvent(input$country, {
    remove <- country_shapes$country_name[!country_shapes$country_name %in% input$country]
    if(length(remove) > 0){
      leafletProxy("worldmap") %>% hideGroup(group = remove)
    }
    if(length(input$country) > 0){
      leafletProxy("worldmap") %>% showGroup(group = input$country)
    }
  }, ignoreNULL = FALSE)
  
  output$table <- DT::renderDataTable( highlighted() )
  
  output$download_button <- renderUI({
    if (!is.null(input$country)) {
      downloadButton("download_file", label = "Download data")
    }
  })
  output$download_file <- downloadHandler(
    filename = "test_download.csv",
    content = function(file) {
      write_csv(highlighted(), file)
    }
  )
  
})