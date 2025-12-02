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

finder <- function(x, l) {
  a <- x[1]
  b <- x[2]
  a_len <- digs(a)
  b_len <- digs(b)
  if (a_len == b_len) {
    if (a_len %% l == 0) {
      fac <- sum(10^seq(0, by = l, length.out = a_len / l))
      low <- ceiling(a / fac)
      high <- floor(b / fac)
      if (low <= high) {
        fac * (low:high)
      }
    }
  } else {
    new_b <- 10^(b_len - 1) - 1
    c(finder(c(a, new_b), l), finder(c(new_b + 1, b), l))
  }
}

apply(input, 1, function(x){finder(x,2)}) %>% unlist() %>% sum() %>% print()