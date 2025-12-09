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
node_inds <- arrayInd(nodes, dims)
node_inds <- node_inds[order(node_inds[, 1]), ]
num_nodes <- nrow(node_inds)

adj_mat <- matrix(0, nrow = num_nodes, ncol = num_nodes)

for (i in seq_along(node_inds[, 1])) {
  below_cond <- node_inds[, 1] > node_inds[i, 1]
  left_cond <- node_inds[, 2] == node_inds[i, 2] - 1
  right_cond <- node_inds[, 2] == node_inds[i, 2] + 1
  below_l <- which(below_cond & left_cond)
  below_r <- which(below_cond & right_cond)
  if (length(below_l) > 0) {
    adj_mat[i, below_l[1]] <- 1
  }
  if (length(below_r) > 0) {
    adj_mat[i, below_r[1]] <- 1
  }
}

visited <- rep(FALSE, nrow(node_inds))

paths <- 0
dfs <- function(node) {
  if (visited[node]) {
    break
  } else {
    if (node > (length(nodes) - nrow(input) + 1)) {
      paths <<- paths + 1
      print(paths)
    }
    visited[node] <- TRUE
    visited_current <- visited
    neighbors <- which(adj_mat[node, ] == 1)
    for (i in neighbors) {
      dfs(i)
      visited <- visited_current
    }
  }
  paths
}

dfs(1) %>% print