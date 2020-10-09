#####################################################
# PRAN - Shiny Demo
# October 2020
# Entferne Ausreißer bzgl. der Distanz
#####################################################

outlier_detection <- function(data, outlier_check) {
  
  if (outlier_check) {
    
    qnt_dist <- quantile(data$dist, probs=c(.25, .75))
    H_dist <- 1.5 * IQR(data$dist)
    data <- data[!data$dist < (qnt_dist[1] - H_dist),]
    data <- data[!data$dist > (qnt_dist[2] + H_dist),]
    
    # qnt_friends <- quantile(data$friends, probs=c(.25, .75))
    # H_friends <- 1.5 * IQR(data$friends)
    # data <- data[!data$friends < (qnt_friends[1] - H_friends),]
    # data <- data[!data$friends > (qnt_friends[2] + H_friends),]
    
  }
  
  return(data)
  
}