library(dplyr)

input_file <- "sample.txt"
len <- readLines(input_file, 1) %>% nchar()
input <- read.table(input_file) %>%
  as.matrix() %>%
  strsplit(split = "") %>%
  unlist() %>%
  matrix(ncol = len, byrow = TRUE)

adjacent_inds <- function(ind, dim) {
  if(ind > dim && ind %% dim > 1 && ind < dim^2 - dim) {
    ad_inds <- c(ind, ind + 1, ind - 1) %>% c(., . + dim, . - dim)
    ad_inds <- setdiff(ad_inds, ind)
  } else if(ind < )
}