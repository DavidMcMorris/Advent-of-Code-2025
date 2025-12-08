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

adj_mat <- matrix(0, nrow = length(input), ncol = length(input))
for (j in seq_len(dims[2])) {
  for (i in 1:(dims[1] - 1)) {
    if (identical(input[i:(i + 1), j], c("|", "|"))) {
      adj_mat[(j - 1) * dims[1] + i, (j - 1) * dims[1] + i + 1] <- 1
    }
  }
}
for (i in seq_len(dims[1])) {
  for (j in 1:(dims[2] - 1)) {
    if (identical(input[i, j:(j + 1)], c("|", "^"))) {
      adj_mat[j * dims[1] + i - 1, (j - 1) * dims[1] + i] <- 1
    } else if (identical(input[i, j:(j + 1)], c("^", "|"))) {
      adj_mat[(j - 1) * dims[1] + i - 1, j * dims[1] + i] <- 1
    }
  }
}

while (sum(visited) < sum(input == "|")) {
  
}