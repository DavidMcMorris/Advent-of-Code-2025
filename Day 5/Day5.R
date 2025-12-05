library(dplyr)

input_file <- "input.txt"
input <- readLines(input_file)
cut <- which(input == "")
ranges <- input[1:(cut - 1)] %>%
  strsplit(., "-") %>%
  unlist() %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  apply(., 2, as.numeric)

ids <- input[(cut + 1):length(input)] %>%
  as.numeric()

rm(input)

counter <- 0

while (length(ids) > 0) {
  for (j in seq_len(nrow(ranges))) {
    if (ids[1] >= ranges[j, 1] && ids[1] <= ranges[j, 2]) {
      counter <- counter + 1
      ids <- ids[-1]
      break
    } else if (j == nrow(ranges)) {
      ids <- ids[-1]
    }
  }
}

print(counter)