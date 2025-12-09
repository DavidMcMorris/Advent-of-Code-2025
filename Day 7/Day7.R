library(dplyr)

input_file <- "sample.txt"
input <- readLines(input_file)
len <- nchar(input[[1]])
input <- input %>%
  strsplit(split = "") %>%
  unlist() %>%
  matrix(ncol = len, byrow = TRUE)
start <- which(input == "S")
input[start] <- "|"
dims <- dim(input)
visited <- rep(0, length(input))

next_line <- function(x, y) {
  for (i in seq_along(x)) {
    if (identical(c(x[i], y[i]), c("|", "."))) {
      y[i] <- "|"
    } else if (identical(c(x[i], y[i]), c("|", "^"))) {
      y[i - 1] <- "|"
      y[i + 1] <- "|"
    }
  }
  y
}

for (i in 1:(dims[1] - 1)) {
  input[i + 1, ] <- next_line(input[i, ], input[i + 1, ])
}

splitters <- which(input == "^")
sum(input[splitters - 1] == "|") %>% print()

nodes <- c(splitters, seq(nrow(input), length(input), by = nrow(input)))
splitter_inds <- arrayInd(nodes, dims)
splitter_inds <- splitter_inds[order(splitter_inds[, 1]), ]
num_nodes <- nrow(splitter_inds)

adj_mat <- matrix(0, nrow = num_nodes, ncol = num_nodes)

for (i in seq_along(splitter_inds[, 1])) {
  below_cond <- splitter_inds[, 1] > splitter_inds[i, 1]
  left_cond <- splitter_inds[, 2] == splitter_inds[i, 2] - 1
  right_cond <- splitter_inds[, 2] == splitter_inds[i, 2] + 1
  below_l <- which(below_cond & left_cond)
  below_r <- which(below_cond & right_cond)
  if (length(below_l) > 0) {
    adj_mat[i, below_l[1]] <- 1
  }
  if (length(below_r) > 0) {
    adj_mat[i, below_r[1]] <- 1
  }
}

visited <- rep(0, nrow(splitter_inds))