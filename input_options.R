#####################################################
# PRAN - Shiny Demo
# October 2020
# Settings
#####################################################

# Eventlistener: Grouping option
observeEvent(input$grouping_enable, {
  RV$grouping_enable <- input$grouping_enable
})

# Eventlistener: Jitter option
observeEvent(input$jitter_enable, {
  RV$jitter_enable <- input$jitter_enable
})

# Eventlistener: Outlier check
observeEvent(input$outlier_check, {
  RV$outlier_check <- input$outlier_check
})

# Font size in Figures
observeEvent(input$plot_base_size, {
  RV$plot_base_size <- input$plot_base_size
})

# Eventlistener: Autoupdate (Settings)
observeEvent(input$autoupdate_interval, {
  RV$autoupdate_interval <- input$autoupdate_interval
})

# Eventlistener: Show Figure Polls
observeEvent(input$show_plot1, {
  shinyjs::toggleElement("plotwrapper_polls", condition = input$show_plot1)
})

# Eventlistener: Show Figure Map
observeEvent(input$show_plot2, {
  shinyjs::toggleElement("plotwrapper_map", condition = input$show_plot2)
})

# Eventlistener: Show Figure Results
observeEvent(input$show_plot3, {
  shinyjs::toggleElement("plotwrapper_results", condition = input$show_plot3)
})
