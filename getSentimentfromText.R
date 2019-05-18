#' Get Sentiment from Text
#' 
#' @param series_of_strings series of strings or column from a dataframe, as characters
#' @param model_cloud .cloud link to model from https://developer.ibm.com/exchanges, as a string
getSentiment <- function(series_of_strings, model_cloud){
  # TO DO
  # Option to pass series of strings in diff formats
  # add try / except for error messages
  
  require(tibble)
  require(dplyr)
  require(httr)
  
  # Model endpoint
  model_endpoint <- paste0(model_cloud, 'model/predict')
  
  # parse strings into json format that model API can read
  series_of_strings <- as.character(series_of_strings)
  series_collapsed <- paste(series_of_strings, collapse = '\", \"')
  series_parsed <- paste0('{ "text": [\"', series_collapsed, '\"]}')
  
  # POST
  response <- httr::POST(model_endpoint, 
                         httr::add_headers(.headers=c(`Content-Type` = 'application/json')), 
                         body = series_parsed, 
                         encode = c("multipart")
                         ) %>% content()
  
  # unpack the response into a tibble 
  output <- tibble(text = series_of_strings, positive = NA, negative = NA)
  position <- 0
  
  for (prediction in response$predictions){
    position <- position + 1
    
    output$positive[position] <- prediction[[1]]$positive
    output$negative[position] <- prediction[[1]]$negative
  }
  
  return(output)
}

## Example

# input <- tibble(text = c("The Model Asset Exchange is a crucial element of a developer\'s toolkit.",
#                          "Have a great Friday!",
#                          "What time do we leave tomorrow?",
#                          "This restaurant is not good."))
# 
# output <- getSentiment(series_of_strings <- input$text,
#              model_cloud = "http://max-text-sentiment-classifier.max.us-south.containers.appdomain.cloud/")
