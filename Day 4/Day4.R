library(dplyr)

input_file <- "sample.txt"
len <- readLines(input_file, 1) %>% nchar()
input <- read.table(input_file) %>%
  as.matrix() %>%
  strsplit(split = "") %>%
  unlist() %>%
  matrix(ncol = len, byrow = TRUE)

adjacent_inds <- function(ind, dim) {
  adj_dirs <- matrix(c(-1, -1, -1, 0, -1, 1, 0, 1), ncol = 2, byrow = TRUE) %>%
    rbind(., -.)
  adj_inds <- apply(adj_dirs, 1, function(x) {x + ind}) %>% t()
  adj_inds[rowSums(adj_inds > dim | adj_inds < 1) == 0, ]
}