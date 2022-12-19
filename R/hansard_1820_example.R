#' The 19th-century British Parliamentary debates for the decade 1820
#'
#' Hansard corpus data for the decade 1820. This data has been pre-formatted to contain words counts by speaker. Stopwords have been removed from the data. To access the raw Hansard data, install the package hansardr. 
#' The variables are as follows:
#'
#' @docType data
#'
#' @usage hansard_1820_example
#'
#' @format A data frame with 1082539 rows and 3 variables:
#'
#' speaker
#' The name of the speaker originally recorded in the transciptions of the debates.
#' 
#' word
#' A word spoken by the given speaker.
#' 
#' n
#' The number of times the given word was spoken by the given speaker
#' 
#' @keywords datasets
#'
#' @examples
#' data(hansard_1820_example)
#'
#' @references Buongiorno, Steph; Kalescky, Robert; Godat, Eric; Cerpa, Omar Alexander; Guldi, Jo (2021) 
#'
#' @source {../data/hansard_1820_example.RData}
#'
"hansard_1820_example"