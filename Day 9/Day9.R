input_file <- "sample.txt"
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

input <- rbind(input, input[1, ])
boundary <- NULL
for (i in 1:(nrow(input) - 1)) {
  d <- which(input[i, ] != input[i + 1, ])
  s <- seq(from = input[i, d], to = input[i + 1, d])
  if (d == 1) {
    boundary <- rbind(boundary, cbind(s[1:(length(s) - 1)], input[i, 2]))
  } else {
    boundary <- rbind(boundary, cbind(input[i, 1], s[1:(length(s) - 1)]))
  }
}
boundary <- rbind(boundary, input[1, ])
