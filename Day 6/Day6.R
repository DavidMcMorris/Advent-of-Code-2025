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

nas <- c(0, which(is.na(input_2)), length(input_2) + 1)
input_2_l <- list()
for (i in 1:(length(nas) - 1)) {
  input_2_l[[i]] <- input_2[(nas[i] + 1):(nas[i + 1] - 1)]
}

apply(input[, -plus], 2, prod) %>% sum(., input[, plus]) %>% print()
sapply(input_2_l[-plus], prod) %>% sum(., unlist(input_2_l[plus])) %>% print()