#####################################################
# PRAN - Shiny Demo
# October 2020
# Scatter Plot
#####################################################

output$results_plot <- renderPlot({
  req(RV$data)
  req(RV$plot_base_size)
  
  if (nrow(RV$data) >= 2) {
    
    position_box_x <- max(table(100*((RV$data %>% outlier_detection(RV$outlier_check) %>% 
                                        pull(dist)+0.01)%/%100 + 
                                       as.logical((RV$data %>% outlier_detection(RV$outlier_check) %>% 
                                                     pull(dist)+0.01)%%100))))/2
    
    position_box_y <- max(table(RV$data %>% outlier_detection(RV$outlier_check) %>% pull(friends)))/2
    
    max_dist <- max(RV$data %>% outlier_detection(RV$outlier_check) %>% pull(dist))
    max_friends <- max(RV$data %>% outlier_detection(RV$outlier_check) %>% pull(friends))
    
    # Grouping on/off
    if (!RV$grouping_enable) {
      
      p_x_y <- ggplot(RV$data %>% outlier_detection(RV$outlier_check), aes(x = dist, y = friends))

      p_x <- ggplot(RV$data %>% outlier_detection(RV$outlier_check), aes(x = dist)) +
        geom_histogram(aes(y = ..count..), binwidth = 100, boundary = 0) +
        geom_boxplot(aes(y = position_box_x), 
                     coef = 0, outlier.shape = NA,
                     # fill = "#A1887F",
                     fill = "transparent",
                     alpha = 0.4,
                     width = position_box_x)
      
      p_y <- ggplot(RV$data %>% outlier_detection(RV$outlier_check), aes(x = friends)) +
        geom_histogram(aes(y = ..count..), binwidth = 0.999, boundary = 0) +
        geom_boxplot(aes(y = position_box_y), 
                     coef = 0, outlier.shape = NA,
                     # fill = "#A1887F",
                     fill = "transparent",
                     alpha = 0.4, 
                     width = position_box_y)
      
      d_text <- RV$data %>% outlier_detection(RV$outlier_check) %>% count()
      p_text <- ggplot(data = d_text, aes(label = paste0("n = ", n))) + 
        geom_text(size = 8, x = 0.5, y = 0.5) +
        theme_void()
      
    } else {
      
      p_x_y <- ggplot(RV$data %>% outlier_detection(RV$outlier_check), 
                      aes(x = dist, y = friends, colour = participation, shape = speaker, alpha = speaker)) +
        scale_colour_manual(values = c("online" = "#34A8E0", 
                                       "vor Ort" = "#F59534")) +
        scale_alpha_manual(values = c("ja" = 0.5, "nein" = 1)) +
        scale_shape_manual(values = c("ja" = 17, "nein" = 16)) +
        guides(colour = guide_legend("Art der Teilnahme", order = 1, nrow = 1, byrow = TRUE,
                                     override.aes = list(size = 6)),
               shape = guide_legend("Vortrag", order = 2, nrow = 1, byrow = TRUE,
                                    override.aes = list(size = 6)),
               alpha = FALSE)

      p_x <- ggplot(RV$data %>% outlier_detection(RV$outlier_check), aes(x = dist, fill = participation)) +
        geom_histogram(aes(y = ..count..), binwidth = 100, boundary = 0) + # position = position_dodge()) +
        geom_boxplot(aes(y = position_box_x,
                         fill = factor(participation, levels = c("vor Ort", "online"))), 
                     coef = 0, outlier.shape = NA,
                     alpha = 0.4,
                     # position = position_dodge(width = 3),
                     width = position_box_x) +
        scale_fill_manual(values = c("online" = "#34A8E0", 
                                     "vor Ort" = "#F59534"))
      
      p_y <- ggplot(RV$data %>% outlier_detection(RV$outlier_check), aes(x = friends, fill = participation)) +
        geom_histogram(aes(y = ..count..), binwidth = 0.999, boundary = 0) + # position = position_dodge()) +
        geom_boxplot(aes(y = position_box_y,
                         fill = factor(participation, levels = c("vor Ort", "online"))),
                     coef = 0, outlier.shape = NA,
                     alpha = 0.4,
                     # position = position_dodge(width = 1.8)
                     width = position_box_y) +
        scale_fill_manual(values = c("online" = "#34A8E0", 
                                     "vor Ort" = "#F59534"))
      
      d_text <- RV$data %>% outlier_detection(RV$outlier_check) %>% count(participation)
      p_text <- ggplot(data = d_text, aes(colour = participation, label = paste0("n = ", n))) + 
        geom_text(size = 8, x = 0.5, y = 0.3 + 0.5*(d_text$participation == "online"), show.legend = FALSE) +
        scale_color_manual(values = c("online" = "#34A8E0", 
                                      "vor Ort" = "#F59534")) +
        theme_void()
    }
    
    p_x_y <- p_x_y + 
      labs("x" = "Distanz (km) des Herkunftsortes von Wien",
           "y" = "Anzahl persönlicher Kontakte zu Data Technology") +
      theme_bw(base_size = RV$plot_base_size) +
      theme(legend.position = "bottom")

    p_x <- p_x +
      scale_y_continuous("Anzahl", breaks = pretty_breaks(n = 4)) + 
      guides(fill = FALSE) +
      theme_bw(base_size = RV$plot_base_size)
  
    p_y <- p_y +
      scale_y_continuous("Anzahl", breaks = pretty_breaks(n = 4)) +
      coord_flip() + 
      guides(fill = FALSE) +
      theme_bw(base_size = RV$plot_base_size)

    # Grouping on/off
    if (!RV$jitter_enable) {
      
      theme_marginal_x <- theme(axis.title.x = element_blank(), 
                                axis.text.x = element_blank(), axis.ticks.x = element_blank())
      theme_marginal_y <- theme(axis.title.y = element_blank(), 
                                axis.text.y = element_blank(), axis.ticks.y = element_blank())

      # dev.new()
      wrap_plots(
        p_x + theme_marginal_x + scale_x_continuous(limits = c(0, max_dist + 100)),
        p_text,
        p_x_y + geom_point(size = 6) +
          scale_x_continuous(limits = c(0, max_dist + 100), breaks = pretty_breaks()) +
          scale_y_continuous(limits = c(0, max_friends + 1), breaks = pretty_breaks()),
        p_y + theme_marginal_y + scale_x_continuous(limits = c(0, max_friends + 1)),
        nrow = 2,
        widths = c(1.8, 0.3),
        heights = c(0.3, 1.8)
      )
      
    } else {
      
      p_x_y <- p_x_y + geom_jitter(size = 6) +
        scale_y_continuous(limits = c(0, max_friends + 1), oob = scales::squish, breaks = pretty_breaks()) +
        scale_x_continuous(limits = c(0, max_dist + 100), oob = scales::squish, breaks = pretty_breaks())
      print(p_x_y)
      
    }
    
  }
})
