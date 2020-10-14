output$model_plot <- renderPlot({

  req(RV$data)
  req(RV$plot_base_size)
  
  if (nrow(RV$data) >= 20) {
  
    # Datenaufbereitung
    data_model <- RV$data
    data_model$num_participation <- as.numeric(data_model$participation)-1
    # data_model_without_out <- outlier_detection(data = data_model, outlier_check = TRUE)
    set.seed(3)
    sample_train <- createDataPartition(data_model$participation, times = 1, p = 0.8, list = FALSE)
    data_model_train <- data_model[sample_train,]
    data_model_test <- data_model[-sample_train,]
    
    # Funktion für Accuracy
    calc_class_err <- function(actual, predicted) {
      mean(actual != predicted)
    }
    
    mod_log <- glm(num_participation ~ dist + friends + speaker, data = data_model_train, family = "binomial")
    
    model_log_pred <- ifelse(predict(mod_log, data_model_test, type = "response") > 0.5, "vor Ort", "online")
    acc_log <- 1 - calc_class_err(actual = data_model_test$participation, predicted = model_log_pred)
    # table(data_model_test$participation, model_log_pred)
    
    # Plotte roc Kurve für Logistische Regression
    test_prob_log <- predict(mod_log, newdata = data_model_test, type = "response")
    base_roc_log <- roc(data_model_test$num_participation ~ test_prob_log, plot = FALSE, print.auc = FALSE)
    roc_log <- ggroc(base_roc_log, alpha = 0.7, colour = "#03A9F4", linetype = 2, size = 2) +
      geom_text(aes(x = 0.05, y = 0.30, label = paste0("AUROC   : ", format(round(base_roc_log$auc, 2), nsmall = 2))), 
                size = 6, colour = "#03A9F4") +
      geom_text(aes(x = 0.05, y = 0.10, label = paste0("Accuracy: ", format(round(acc_log, 2), nsmall = 2))), 
                size = 6, colour = "#03A9F4") +
      geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed", size = 1.2) +
      ggtitle("ROC-Kurve für <span style='color:#03A9F4;'>Logistische Regression</span>",
              subtitle = "Modell: Art der Teilnahme ~ Distanz + Anzahl Kontakte + Vortrag") +
      xlab("Spezifität") +
      ylab("Sensitivität") +
      theme_minimal(base_size = RV$plot_base_size - 2) +
      theme(plot.title = element_markdown(lineheight = 1.0))
    
    # Classification Tree
    mod_tree <- rpart(participation ~ dist + friends + speaker, data = data_model_train,
                      control = rpart.control(cp = 0.01))
    
    acc_tree <- 1 - calc_class_err(actual = data_model_test$participation, predicted = predict(mod_tree, newdata = data_model_test, type = "class"))
    # table(data_model_test$participation, predict(mod_tree, newdata = data_model_test, type = "class"))
    
    # Plotte roc Kurve für Classification Tree
    test_prob_tree <- predict(mod_tree, newdata = data_model_test, type = "prob")[,2]
    base_roc_tree <- roc(data_model_test$participation ~ test_prob_tree, plot = FALSE, print.auc = FALSE)
    roc_tree <- ggroc(base_roc_tree, alpha = 0.7, colour = "#D84315", linetype = 2, size = 2) +
      geom_text(aes(x = 0.05, y = 0.30, label = paste0("AUROC   : ", format(round(base_roc_tree$auc, 2), nsmall = 2))), 
                size = 6, colour = "#D84315") +
      geom_text(aes(x = 0.05, y = 0.10, label = paste0("Accuracy: ", format(round(acc_tree, 2), nsmall = 2))), 
                size = 6, colour = "#D84315") +
      geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed", size = 1.2) +
      ggtitle("ROC-Kurve für <span style='color:#D84315;'>Classification Tree</span>",
              subtitle = "Prädiktoren: Distanz, Anzahl Kontakte, Vortrag") +
      xlab("Spezifität") +
      ylab("Sensitivität") +
      theme_minimal(base_size = RV$plot_base_size - 2) +
      theme(plot.title = element_markdown(lineheight = 1.0),
            plot.margin = unit(c(1.75,0,0,0), "cm"))
    
    # Random Forest
    set.seed(9)
    mod_rf <- randomForest(participation ~ dist + friends + speaker, 
                           ntree = 500,
                           data = data_model_train)
   
    model_rf_pred <- ifelse(predict(mod_rf, newdata = data_model_test, type = "prob")[,2] > 0.5, "vor Ort", "online")
    acc_rf <- 1 - calc_class_err(actual = data_model_test$participation, predicted = model_rf_pred)
    # table(data_model_test$participation, model_rf_pred)
    # table(model_log_pred, model_rf_pred, predict(mod_tree, newdata = data_model_test, type = "class"))
    
    # Plotte roc Kurve für Random Forest
    test_prob_rf <- predict(mod_rf, newdata = data_model_test, type = "prob")[,2]
    base_roc_rf <- roc(data_model_test$participation ~ test_prob_rf, plot = FALSE, print.auc = FALSE)
    roc_rf <- ggroc(base_roc_rf, alpha = 0.7, colour = "#43A047", linetype = 2, size = 2) +
      geom_text(aes(x = 0.05, y = 0.30, label = paste0("AUROC   : ", format(round(base_roc_rf$auc, 2), nsmall = 2))), 
                size = 6, colour = "#43A047") +
      geom_text(aes(x = 0.05, y = 0.10, label = paste0("Accuracy: ", format(round(acc_rf, 2), nsmall = 2))), 
                size = 6, colour = "#43A047") +
      geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed", size = 1.2) +
      ggtitle("ROC-Kurve für <span style='color:#43A047;'>Random Forest Modell</span>",
              subtitle = "Prädiktoren: Distanz, Anzahl Kontakte, Vortrag") +
      xlab("Spezifität") +
      ylab("Sensitivität") +
      theme_minimal(base_size = RV$plot_base_size - 2) +
      theme(plot.title = element_markdown(lineheight = 1.0),
            plot.margin = unit(c(1.75,0,0,0), "cm"))
    
    # Plotte alle drei Abbildungen untereinander
    roc_log / roc_tree / roc_rf
  }
}
)
  





