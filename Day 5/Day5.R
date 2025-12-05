library(dplyr)

input_file <- "sample.txt"
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

# counter <- 0

# while (length(ids) > 0) {
#   for (j in seq_len(nrow(ranges))) {
#     if (ids[1] >= ranges[j, 1] && ids[1] <= ranges[j, 2]) {
#       counter <- counter + 1
#       ids <- ids[-1]
#       break
#     } else if (j == nrow(ranges)) {
#       ids <- ids[-1]
#     }
#   }
# }
# print(counter)


overlap <- function(x, y) {
  if (x[1] >= y[1] && x[1] <= y[2]) {
    TRUE
  } else if (x[2] >= y[1] && x[2] <= y[2]) {
    TRUE
  } else {
    FALSE
  }
}

i <- 0
n <- nrow(ranges)
while (i < (n - 2)) {
  i <- i + 1
  j <- i + 1
  n <- nrow(ranges)
  while (j <= n) {
    if (overlap(ranges[j, ], ranges[i, ])) {
      ranges[i, ] <- c(min(ranges[c(i, j), 1]), max(ranges[c(i, j), 2]))
      ranges <- ranges[-j, ]
      j <- j - 1
      n <- n - 1
    } else {
      j <- j + 1
    }
  }
}

(ranges[, 2] - ranges[, 1] + 1) %>% sum() %>% print()