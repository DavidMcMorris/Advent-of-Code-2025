library(dplyr)

input_file <- "input.txt"
len <- readLines(input_file, 1) %>% nchar()
input <- read.table(input_file) %>%
  as.matrix() %>%
  strsplit(split = "") %>%
  unlist() %>%
  matrix(ncol = len, byrow = TRUE)