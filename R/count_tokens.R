#' Count Tokens
#' @description Converts a data frame with columns for text and grouping variables into a data frame with each word and the count of each word in each group.
#' @param data Data frame containing the raw data
#' @param group The name of the column(s) containing the grouping variable. If not defined, the text will not be grouped. Can be given as either a string or a vector of strings.
#' @param text The name of the column containing the text that needs to be tokenized
#' @return Data frame containing columns for the word, the group(s) and the count labeled as 'word', the group name, and 'n'
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr group_by_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr count
#' @importFrom tidytext unnest_tokens
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
#' count_tokens(test, text = "myText", group = "myGroup")

count_tokens = function(data, group = NA, text = "text") {
  # Remove note
  word = NULL
  . = NULL
  
  if (is.na(group)[1]) {
    # If group not defined, group by nothing
    newData = data %>%
      unnest_tokens(word, !!text) %>%
      count(word, sort = TRUE)
  } else {
    # Define the columns to keep
    cols = c(group, text)
    # Group by all kept columns
    newData = data[, cols] %>%
      unnest_tokens(word, !!text) %>%
      group_by_all() %>%
      count(word, sort = TRUE) %>%
      ungroup()
  }
  
  newData = as.data.frame(newData)
  return(newData)
}
