#####################################################
# PRAN - Shiny Demo
# October 2020
# Time Series Plot of submitted polls
#####################################################

output$ts_plot <- renderPlot({
  req(RV$ts_data)
  req(RV$plot_base_size)
  
  if (nrow(RV$ts_data) > 2) {
    
    p <- ggplot(RV$ts_data, aes(x = datestamp, y = cumsum, group = complete)) +
      geom_hline(yintercept = 0, colour = "#9E9E9E") +
      geom_step(aes(colour = complete), size = 2, direction = "hv") +
      scale_colour_manual(values = c("#03A9F4", "grey"), breaks = c("vollständig", "unvollständig"), name = NULL) +
      scale_y_continuous(limits = c(0, max(unique(floor(pretty(seq(0, (max(RV$ts_data$cumsum) + 1) * 1.1)))))),
                         breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      scale_x_datetime(date_breaks = "1 day") +
      theme_minimal(base_size = RV$plot_base_size) +
      labs(
        y = "Anzahl eingelangter Fragebögen",
        x = "Datum/Uhrzeit"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.justification = c(1, 0),
        legend.position = c(0.20, 0.85)
      )
    
    print(p)
    
  }
})
