#' @export
jsd_score <- function(p, m) {
  p* log(p/m) }

#' @export
partial_jsd <- function(p, q) {
  p * log(p/((p+q)/2)) }

#' @export
find_jsd <- function(tokenized_hansard_1850, tokenized_hansard_1860, corpus) {

  tokenized_hansard_1850 <- tokenized_hansard_1850 %>%
    select(word, corpus)

  tokenized_hansard_1860 <- tokenized_hansard_1860 %>%
    select(word, corpus)

  corpus <- dplyr::enquo(corpus)

  x1 <- tokenized_hansard_1850 %>%
    dplyr::count(word, !!corpus)

  corpus_1 <- as.character(tokenized_hansard_1850[1,2])

  x1 <-  dplyr::mutate(x1, "prob_{corpus_1}" := philentropy::estimate.probability(n))

  x2 <- tokenized_hansard_1860 %>%
    dplyr::count(word, !!corpus)

  corpus_2 <- as.character(tokenized_hansard_1860[1,2])
  x2 <-  dplyr::mutate(x2, "prob_{corpus_2}" := philentropy::estimate.probability(n))

  df1 <- x1 %>%
    dplyr::full_join(x2, by = "word") %>%
    dplyr::mutate_all(funs(replace(., is.na(.), 0)))

  #df1 <- df1[, c(1, 4, 7)]
  df1 <- df1[, c(1, 3, 5)]

  j <- partial_jsd(df1[,2], df1[,3])
  k <- partial_jsd(df1[,3], df1[,2])

  colnames(j)[1] <- paste0("jsd_", corpus_1)
  colnames(k)[1] <- paste0("jsd_", corpus_2)

  everything <- cbind(df1, j, k)

  return(everything) }
