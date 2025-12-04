library(dplyr)

input_file <- "input.txt"
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

total_acc <- 0
for (i in 1:len^2) {
  if (input[i] == "@") {
    current_ind <- arrayInd(i, c(len, len))
    adj_inds <- adjacent_inds(current_ind, len)
    adj <- (input[adj_inds] == "@") %>% sum()
    if (adj < 4) {
      total_acc <- total_acc + 1
    }
  }
}
print(total_acc)

total_rem <- 0
flag <- 1
while (flag == 1) {
  flag <- 0
  for (i in 1:len^2) {
    if (input[i] == "@") {
      current_ind <- arrayInd(i, c(len, len))
      adj_inds <- adjacent_inds(current_ind, len)
      adj <- (input[adj_inds] == "@") %>% sum()
      if (adj < 4) {
        input[i] <- "."
        total_rem <- total_rem + 1
        flag <- 1
      }
    }
  }
}
print(total_rem)