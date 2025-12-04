library(dplyr)

input_file <- "input.txt"

digs <- function(x) {
  floor(log10(x)) + 1
}

base_10 <- function(x) {
  tens <- rev(10^seq(0, length(x) - 1))
  sum(x * tens)
}

len <- scan(input_file, nmax = 1) %>% digs()
input <- readLines(input_file) %>%
  strsplit(split = "") %>%
  unlist() %>%
  as.numeric() %>%
  matrix(ncol = len, byrow = TRUE)

next_bat <- function(x, l) {
  a <- max(x[1:(length(x) - l + 1)])
  if (l == 1) {
    a
  } else {
    a_ind <- which(x == a)[1]
    c(a, next_bat(x[-(1:a_ind)], l - 1))
  }
}

total <- c(0, 0)
for (i in seq_len(nrow(input))) {
  total[1] <- total[1] + (next_bat(input[i, ], 2) %>% base_10())
  total[2] <- total[2] + (next_bat(input[i, ], 12) %>% base_10())
}
print(total, digits = 13)
