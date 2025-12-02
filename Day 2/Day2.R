library(dplyr)

input_file <- "sample.txt"
input <- read.csv(input_file, header = FALSE) %>%
  as.character() %>%
  strsplit(., "-") %>%
  unlist() %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  apply(., 2, as.numeric)

finder <- function(a, b) {
  a_len <- nchar(a)
  b_len <- nchar(b)
  if (a_len == b_len) {
    fac <- 10^(a_len / 2) + 1
    low <- ceiling(a / fac)
    high <- floor(b / fac)
    fac * (low:high)
  }
}