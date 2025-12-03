library(dplyr)

input_file <- "input.txt"
input <- read.csv(input_file, header = FALSE) %>%
  as.character() %>%
  strsplit(., "-") %>%
  unlist() %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  apply(., 2, as.numeric)

digs <- function(x) {
  floor(log10(x)) + 1
}

finder <- function(x, l, p) {
  a <- x[1]
  b <- x[2]
  a_len <- digs(a)
  b_len <- digs(b)
  if (a_len == b_len) {
    if (p == 1) {
      l <- a_len / 2
      n <- 2
    } else {
      n <- a_len / l
    }
    if (a_len <= l && p == 2) {
      NA
    } else if ((a_len %% l == 0 && p == 2) || (a_len %% 2 == 0 && p == 1)) {
      fac <- sum(10^seq(0, by = l, length.out = n))
      low <- ceiling(a / fac)
      high <- floor(b / fac)
      if (low <= high) {
        fac * (low:high)
      }
    }
  } else {
    new_b <- 10^(b_len - 1) - 1
    c(finder(c(a, new_b), l, p), finder(c(new_b + 1, b), l, p))
  }
}

apply(input, 1, function(x) {finder(x, 0, 1)}) %>% unlist() %>% sum() %>% print()

m <- floor(digs(max(input)) / 2)
ids <- NULL
for (i in 1:m) {
  ids <- apply(input, 1, function(x) {finder(x, i, 2)}) %>%
    unlist() %>%
    union(., ids)
}
print(sum(ids, na.rm = TRUE))