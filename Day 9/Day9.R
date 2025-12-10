input_file <- "input.txt"
input <- read.csv(input_file, header = FALSE) |> as.matrix()

area <- function(a, b) {
  dx <- abs(a[1] - b[1]) + 1
  dy <- abs(a[2] - b[2]) + 1
  dx * dy
}

max_area <- 0
for (i in seq_len(nrow(input))) {
  for (j in seq_len(nrow(input))) {
    max_area <- max(max_area, area(input[i, ], input[j, ]))
  }
}