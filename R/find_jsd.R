#' @export
jsd <- function(p, m) {
  p* log(p/m) }

#' @export
partial_jsd <- function(p, q) {
  p * log(p/((p+q)/2)) }

#' @export
find_jsd <- function(tokenized_hansard_1850, tokenized_hansard_1860, period) {

  x1 <- tokenized_hansard_1850 %>%
    dplyr::count(word, period)
  period_1 <- as.character(x1[1,2])
  x1 <-  dplyr::mutate(x1, "prob_{period_1}" := philentropy::estimate.probability(n))


  x2 <- tokenized_hansard_1860 %>%
    dplyr::count(word, period)
  period_2 <- as.character(x2[1,2])
  x2 <-  dplyr::mutate(x2, "prob_{period_2}" := philentropy::estimate.probability(n))

  df1 <- x1 %>%
    dplyr::full_join(x2, by = "word") %>%
    dplyr::mutate_all(funs(replace(., is.na(.), 0)))

  df1 <- df1[, c(1, 4, 7)]

  j <- partial_jsd(df1[,2], df1[,3])
  k <- partial_jsd(df1[,3], df1[,2])

  colnames(j)[1] <- paste0("jsd_", period_1)
  colnames(k)[1] <- paste0("jsd_", period_2)

  everything <- cbind(df1, j, k)

  return(everything) }
