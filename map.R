#####################################################
# PRAN - Shiny Demo
# October 2020
# Map with Leaflet
#####################################################

output$map <- renderLeaflet({
  
  req(RV$data)
  req(input$show_plot2)

  icons_color <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor =  ifelse(RV$data$participation == "online", "blue", "orange")
  )
  
  if (!RV$jitter_enable) { 
    
    leaflet(data = RV$data %>% outlier_detection(RV$outlier_check)) %>%
      addTiles() %>%
      addAwesomeMarkers(~long, ~lat, popup = ~popup, label = ~label, icon = icons_color) %>%
      addLegend(position = "bottomright",
                colors = c("#34A8E0", "#F59534"),
                labels = c("online", "vor Ort"), 
                opacity = 1,
                title = "Art der Teilnahme")

  } else {
    
    leaflet(data = RV$data %>%
              outlier_detection(RV$outlier_check) %>%
              mutate(lat = jitter(lat, factor = 80),
                     long = jitter(long, factor = 80))) %>%
      addTiles() %>%
      addAwesomeMarkers(~long, ~lat, popup = ~popup, label = ~label, icon = icons_color) %>%
      addLegend(position = "bottomright",
                colors = c("#34A8E0", "#F59534"),
                labels = c("online", "vor Ort"), 
                opacity = 1,
                title = "Art der Teilnahme")
  }
    
})