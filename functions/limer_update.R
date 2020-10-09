#####################################################
# PRAN - Shiny Demo
# October 2020
# Limer Update for latest release
#####################################################

# Overwrite limer functions (necessary for latest LimeSurvey Release)
base64_to_df <- function(x, sep=";") {
  raw_csv <- rawToChar(base64enc::base64decode(x))
  return(read.csv(textConnection(raw_csv), stringsAsFactors = FALSE, sep = sep))
}
get_responses <- function(iSurveyID, sDocumentType = "csv", sLanguageCode = NULL,
                          sCompletionStatus = "complete", sHeadingType = "code",
                          sResponseType = "long", sep=";", ...) {
  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())
  dots <- list(...)
  if(length(dots) > 0) params <- append(params,dots)
  # print(params) # uncomment to debug the params
  
  results <- call_limer(method = "export_responses", params = params)
  return(base64_to_df(unlist(results), sep=sep))
}