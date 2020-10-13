#####################################################
# PRAN - Shiny Demo
# October 2020
# shinyServer File
#####################################################

# Package path
package_path <- "/home/ubuntu/R/x86_64-pc-linux-gnu-library/4.0"

# Loading packages
library(limer, lib.loc =  package_path)
library(ggplot2, lib.loc =  package_path)
library(scales, lib.loc =  package_path)
library(geosphere, lib.loc =  package_path)
library(leaflet, lib.loc =  package_path)
library(htmlwidgets, lib.loc =  package_path)
library(shinyjs, lib.loc =  package_path)
library(tidyr, lib.loc = package_path)
library(dplyr, lib.loc = package_path)
library(patchwork, lib.loc = package_path)
library(pROC, lib.loc =  package_path)
library(randomForest, lib.loc =  package_path)
library(rpart, lib.loc =  package_path)
library(caret, lib.loc =  package_path)
library(ggtext, lib.loc =  package_path)

# Source R functions
source("functions/ts_data_preparation.R", local = TRUE)
source("functions/data_preparation.R", local = TRUE)
source("functions/outlier_detection.R", local = TRUE)
source("functions/limer_update.R", local = TRUE)

# Source credentials: lime_user, lime_pw, api_url and survey_id (not on github)
source("credentials.R", local = TRUE)

shinyServer(function(input, output, session) {
  
  # Enhance shiny with java script functionality
  shinyjs::useShinyjs(html = TRUE)

  # Access Options for LimeSurvey (defined in credentials.R)
  options(lime_api = api_url)
  options(lime_username = lime_user)
  options(lime_password = lime_pw)
  
  # LimeSurvey Log in
  get_session_key()

  # Initialise Reactive Values
  RV <- reactiveValues(data = NULL, ts_data = NULL, autoupdate_interval = NULL)

  # Initialise Progress bar
  t <- 0
  progress <- Progress$new(session, min = 0, max = 10)
  progress$set(message = "Automatisches aktualisieren")

  # First time loading of LimeSurvey data via limer
  responses <- tryCatch(
    get_responses(
      survey_id,
      sDocumentType = "csv",
      sCompletionStatus = "all",
      sHeadingType = "code",
      sResponseType = "long"
    ),
    error = function(e) NULL
  )

  if (!is.null(responses)) {
    if (nrow(responses) == 0) {
      responses <- NULL
    }
  }

  # Data Preparation
  RV$ts_data <- ts_data_preparation(responses)
  RV$data <- data_preparation(responses)
  
  # OUTPUTS
  source("timeseries.R", local = TRUE)
  source("map.R", local = TRUE)
  source("results.R", local = TRUE)
  source("model.R", local = TRUE)

  # INPUTS
  source("input_progress.R", local = TRUE)
  source("input_options.R", local = TRUE)
  
})
