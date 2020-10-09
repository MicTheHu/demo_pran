#####################################################
# PRAN - Shiny Demo
# October 2020
# Data preparation
#####################################################

data_preparation <- function(data) {

  if (!is.null(data)) {
    
    # Take complete data sets only
    data <- data[data$submitdate != "" & !is.na(data$submitdate),]
    
    # Prepare participation category
    data$participation <- factor(data$participation,
                               levels = c("online", "vor Ort im Tech Gate Vienna (zumindest an einem Tag)"),
                               labels = c("online", "vor Ort"))
    
    # Prepare speaker/listener
    data$speaker <- factor(data$speaker,
                           levels = c("ja", "nein"))
    # browser()
    # Map Data Preparation
    data <- data %>% 
      separate(hometown, into = c("lat", "long"), sep = ";", convert = TRUE)
    # long, lat
    loc <- c(16.41237, 48.23312)

    for (i in 1:nrow(data)) {
      if (!is.na(data[i, "lat"])) {
        data[i,"dist"] <- distHaversine(data[i, c("long", "lat")], loc)/1000
      }
    }
    data[, "label"] <- paste0("Distanz: ", round(data$dist, 0), "km")
    data[, "popup"] <- paste0("Distanz: ", '<strong>', round(data$dist, 0), " km <br>", '</strong>',
                              "Art der Teilnahme: ", '<strong>', data$participation, "<br>", '</strong>', 
                              "Anzahl Kontakte zu DT: ", '<strong>', data$friends, " <br>", '</strong>', 
                              "Vortrag: ", '<strong>', data$speaker, '</strong>')
    
  } else {
    data <- data.table(
      "id" = integer(),
      "submitdate" = character(),
      "lastpage" = integer(),
      "startlanguage" = character(),
      "seed" = integer(),
      "startdate" = character(),
      "datestamp" = character(),
      "ipaddr" = character(),
      "participation" = factor(),
      "speaker" = factor(),
      "friends" = integer(),
      "lat" = numeric(),
      "long" = numeric(),
      "dist" = numeric(),
      "label" = character(),
      "popup" = character()
    )
  }
  return(data)
}