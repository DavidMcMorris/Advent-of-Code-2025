library(dplyr)

input_file <- "input.txt"
input <- read.table(input_file)
ops <- input[nrow(input), ]
input <- input[-nrow(input), ] %>% apply(., 2, as.numeric)

plus <- which(ops == "+")

apply(input[, -plus], 2, prod) %>% sum(., input[, plus]) %>% print(digits = 20)