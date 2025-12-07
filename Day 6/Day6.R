library(dplyr)

input_file <- "input.txt"
options(digits = 20)
input <- read.table(input_file)
ops <- input[nrow(input), ]
input <- input[-nrow(input), ] %>% apply(., 2, as.numeric)
plus <- which(ops == "+")

input_2 <- readLines(input_file)
input_2 <- input_2[-length(input_2)] %>% strsplit(split = "")
len <- length(input_2[[1]])
input_2 <- input_2 %>%
  unlist() %>%
  matrix(ncol = len, byrow = TRUE) %>%
  apply(., 2, paste, collapse = "") %>%
  as.numeric()

n <- which(is.na(input_2))[1] - 1
input_2 <- input_2[which(!is.na(input_2))] %>% matrix(., nrow = n)

apply(input[, -plus], 2, prod) %>% sum(., input[, plus]) %>% print()
apply(input_2[, -plus], 2, prod) %>% sum(., input_2[, plus]) %>% print()