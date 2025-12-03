library(dplyr)

digs <- function(x) {
  floor(log10(x)) + 1
}

input_file <- "sample.txt"
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

total <- 0
for (i in seq_len(nrow(input))) {
  a <- max(input[i, -len])
  a_ind <- which(input[i, ] == a)[1]
  b <- max(input[i, -(1:a_ind)])
  total <- total + a * 10 + b
}
print(total)
