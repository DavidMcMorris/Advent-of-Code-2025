library(dplyr)

input_file <- "input.txt"
input <- readLines(input_file)
len <- nchar(input[[1]])
input <- input %>%
  strsplit(split = "") %>%
  unlist() %>%
  matrix(ncol = len, byrow = TRUE)

input[input == "S"] <- "|"

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

for (i in 1:(nrow(input) - 1)) {
  input[i + 1, ] <- next_line(input[i, ], input[i + 1, ])
}

splitters <- which(input == "^")
sum(input[splitters - 1] == "|") %>% print()