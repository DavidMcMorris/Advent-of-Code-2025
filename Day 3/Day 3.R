library(dplyr)

digs <- function(x) {
  floor(log10(x)) + 1
}

input_file <- "input.txt"
len <- scan(input_file, nmax = 1) %>% digs()
input <- readLines(input_file) %>%
  strsplit(split = "") %>%
  unlist() %>%
  as.numeric() %>%
  matrix(ncol = len, byrow = TRUE)

total <- 0
for (i in seq_len(nrow(input))) {
  a <- max(input[i, -len])
  a_ind <- which(input[i, ] == a)[1]
  b <- max(input[i, -(1:a_ind)])
  total <- total + a * 10 + b
}
print(total)

jolt <- function(x)
  y <- NULL
  for (i in 1:12) {
    a <- max(x[1:(len - 13 + i)])
    a_ind <- which(input[i, ] == a)[1]
  }