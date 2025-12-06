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

n <- nrow(ranges)
i <- 1
while (i < n) {
  ind_rem <- NULL
  for (j in (i + 1):n) {
    if (overlap(ranges[j, ], ranges[i, ])) {
      ranges[i, ] <- c(min(ranges[c(i, j), 1]), max(ranges[c(i, j), 2]))
      ind_rem <- c(ind_rem, j)
    }
  }
  if (length(ind_rem) > 0) {
    ranges <- ranges[-ind_rem, ]
    n <- nrow(ranges)
  }
  i <- i + 1
}

(ranges[, 2] - ranges[, 1] + 1) %>% sum() %>% print()