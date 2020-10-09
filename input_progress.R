#####################################################
# PRAN - Shiny Demo
# October 2020
# Automatic update feature
#####################################################

# Automatic Update Feature
observe({
  req(RV$autoupdate_interval)
  
  if (input$autoupdate_enable == TRUE) {
    
    # Invalidate after a tenth of total time interval for automatic updates (in seconds) elapsed
    invalidateLater(RV$autoupdate_interval * 100, session)
    
    # Increment t by 1 (modify t with scoping assignment, because t is defined in parent environment)
    t <<- t + 1

    # Load LimeSurvey data if t > 10
    if (t > 10) {
      
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

      # Set t to 0 - Progress bar starts again
      t <<- 0

    }
    # Update progress bar, take value of t
    progress$set(t)
  }
})

# Automatic Logins in LimeSurvey, not to get logged out
# only every 10 minutes
observe({
  if (input$autoupdate_enable == TRUE) {
    invalidateLater(6e+05, session)
    # LimeSurvey Log in
    get_session_key()  
  }
})
