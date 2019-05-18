#' Get Sentiment from Text
#' 
#' @param series_of_strings series of strings, as characters
#' @param model_cloud .cloud link to model from https://developer.ibm.com/exchanges, as a string
getSentiment <- function(series_of_strings, model_cloud){
  # TO DO
  # Option to pass series of strings in diff formats
  
  # Model endpoint
  model_endpoint <- paste0(model_cloud, 'model/predict')
  
  series_collapsed <- paste(series_of_strings, collapse = '\", \"')
  series_parsed <- paste0('{ "text": [\"', series_collapsed, '\"]}')
  
  # POST
  response <- httr::POST(model_endpoint, 
                         httr::add_headers(.headers=c(`Content-Type` = 'application/json')), 
                         body = data_series, 
                         encode = c("multipart")
                         ) %>% content()
  
  # parse the response into a tibble ----------------
  output <- tibble(text = series_of_strings, positive = NA, negative = NA)
  position <- 0
  
  for (prediction in response$predictions){
    
    position <- position + 1
    
    output$positive[position] <- prediction[[1]]$positive
    output$negative[position] <- prediction[[1]]$negative
  }
  
  return(output)
}