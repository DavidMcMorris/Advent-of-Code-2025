library(dplyr)

input_file <- "sample.txt"
input <- readLines(input_file)
cut <- which(input == "")
ranges <- input[1:(cut - 1)] %>%
  strsplit(., "-") %>%
  unlist() %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  apply(., 2, as.numeric)

ids <- input[(cut + 1):length(input)] %>%
  as.numeric()

rm(input)