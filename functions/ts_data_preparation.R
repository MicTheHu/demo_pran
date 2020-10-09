#####################################################
# PRAN - Shiny Demo
# October 2020
# Data preparation for time series plot
#####################################################

ts_data_preparation <- function(data, startdate = as.POSIXct("2020-10-12 11:00:00", format = "%Y-%m-%d %H:%M:%S")) {
  
  if (!is.null(data)) {
    
    data$complete <- ifelse(data$submitdate == "" | is.na(data$submitdate), "unvollständig", "vollständig")
    data <- as.data.frame(table(data$datestamp, data$complete))
    data <- data[data$Freq != 0,]
    names(data) <- c("datestamp", "complete", "N")
    data$cumsum <- do.call(c, as.list(tapply(data$N, data$complete, cumsum)))
    data$datestamp <- as.POSIXct(data$datestamp, format = "%Y-%m-%d %H:%M:%S")
    # Add maximum value
    max <- rbind(data.frame("datestamp" = Sys.time()+(3600*2), "complete" = "vollständig", "N" = 0, "cumsum" = max(c(data$cumsum[data$complete == "vollständig"], 0))),
                 data.frame("datestamp" = Sys.time()+(3600*2), "complete" = "unvollständig", "N" = 0, "cumsum" = max(c(data$cumsum[data$complete == "unvollständig"], 0))))
    data <- rbind(data, max)
    
  } else {
    
    data <- data.frame(datestamp = character(), complete = character(), N = integer(), cumsum = integer())
    
  }
  
  # add startdates
  data <- rbind(
    data.frame("datestamp" = startdate, "complete" = "unvollständig", "N" = 0, "cumsum" = 0),
    data.frame("datestamp" = startdate, "complete" = "vollständig", "N" = 0, "cumsum" = 0),
    data
  )
  
  return(data)
}
