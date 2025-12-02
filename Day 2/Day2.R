library(dplyr)

input_file <- "sample.txt"
input <- read.csv(input_file, header = FALSE) %>%
  as.character() %>%
  strsplit(., "-") %>%
  unlist() %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  apply(., 2, as.numeric)

digs <- function(x) {
  floor(log10(x)) + 1
}

finder <- function(x) {
  a <- x[1]
  b <- x[2]
  a_len <- digs(a)
  b_len <- digs(b)
  if (a_len == b_len) {
    if (a_len %% 2 == 0) {
      fac <- 10^(a_len / 2) + 1
      low <- ceiling(a / fac)
      high <- floor(b / fac)
      fac * (low:high)
    } else {
      NA
    }
  } else {
    new_b <- 10^(b_len - 1) - 1
    c(finder(c(a, new_b)), finder(c(new_b + 1, b)))
  }
}