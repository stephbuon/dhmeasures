#' Remove Stop Words
#' @description Remove stop words and fully numeric words from a column in a dataframe that contains words. It is assumed that the text has been tokenized (see dhmeasures::tokenize_counts) prior to using this function.
#' @param data Data frame containing your data
#' @param words The name of the column where stop words should be searched for
#' @param stop_words Vector of stop words. Uses dhmeasures::stop_word as the default
#' @param remove_numbers Set to true (default) to remove all numeric values from the words column
#' @return Data frame with all prior data, but without rows containing stop words
#' @importFrom dplyr %>%
#' @importFrom dplyr anti_join
#' @importFrom dplyr filter
#' @examples 
#' test = data.frame (
#'   myText = c(
#'     "Hello! This is the first sentence I am using to test this function!",
#'     "This is the second sentence!"
#'   ),
#'   myGroup = c(
#'     "group1",
#'     "group2"
#'   )
#' )
#' 
#' test2 = count_tokens(test, text = "myText", group = "myGroup")
#' test2
#' 
#' remove_stop_words(test2)

remove_stop_words = function(data, words = "word", stop_words = dhmeasures::stop_word, remove_numbers = TRUE) {
  # Remove note
  . = NULL
  
  # Reformat stop_words
  stopwords.df = data.frame (
    stop_words
  )
  names(stopwords.df) = c(words)
  
  # Remove numbers from words column
  if (remove_numbers) {
    data = data %>%
      filter(!grepl("[0-9]", .[[words]]))
  }
  
  # Remove stopwords and empty words (including fully numeric)
  data = data %>%
    anti_join(stopwords.df, by = words) %>%
    filter(.[[words]] != "")
  
  return(data)
}


