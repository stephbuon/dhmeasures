#' Remove Stop Words
#' @description Remove stop words and fully numeric words from a column in a dataframe that contains words. It is assumed that the text has been tokenized (see dhmeasures::tokenize_counts) prior to using this function.
#' @param data Data frame containing your data
#' @param words The name of the column where stop words should be searched for
#' @param stop_words Vector of stop words. Uses dhmeasures::stop_word as the default
#' @return Data frame with all prior data, but without rows containing stop words
#' @importFrom dplyr %>%
#' @importFrom dplyr anti_join
#' @importFrom dplyr filter
#' @examples 
#' # Load example Hansard 1820 dataset
#' data(hansard_1820_example)
#' head(hansard_1820_example)
#' 
#' head(remove_stop_words(hansard_1820_example))

remove_stop_words = function(data, words = "word", stop_words = dhmeasures::stop_word) {
  # Reformat stopw_words
  stopwords.df = data.frame (
    stop_words
  )
  names(stopwords.df) = c(words)
  
  # Remove numbers from words column
  data[[words]] = gsub("[0-9]", "", data[[words]])
  
  # Remove stopwords and empty words (including fully numeric)
  newData = data %>%
    anti_join(stopwords.df, by = words) %>%
    filter(.[[words]] != "")
  
  return(newData)
}


