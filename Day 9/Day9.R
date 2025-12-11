input_file <- "sample.txt"
input <- read.csv(input_file, header = FALSE) |> as.matrix()

area <- function(a, b) {
  dx <- abs(a[1] - b[1]) + 1
  dy <- abs(a[2] - b[2]) + 1
  dx * dy
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

area_bound <- function(a, b) {
  if (a[1] == b[1]) {
    coord_mat <- cbind(a[1], a[2]:b[2])
  } else if (a[2] == b[2]) {
    coord_mat <- cbind(a[1]:b[1], a[2])
  } else {
    ne <- c(max(a[1], b[1]), min(a[2], b[2]))
    nw <- c(min(a[1], b[1]), ne[2])
    se <- c(ne[1], max(a[2], b[2]))
    sw <- c(nw[1], se[2])
    coord <- ne
    coord_mat <- coord
    while (!identical(coord, ne + c(0, 1))) {
      if (coord[2] == nw[2] && coord[1] > nw[1]) {
        coord[1] <- coord[1] - 1
      } else if (coord[1] == nw[1] && coord[2] >= nw[2] && coord[2] < sw[2]) {
        coord[2] <- coord[2] + 1
      } else if (coord[2] == sw[2] && coord[1] >= sw[1] && coord[1] < se[1]) {
        coord[1] <- coord[1] + 1
      } else if (coord[1] == se[1] && coord[2] <= se[2]) {
        coord[2] <- coord[2] - 1
      }
      coord_mat <- rbind(coord_mat, coord, deparse.level = 0)
    }
  }
  coord_mat
}

max_area <- 0
for (i in 1:(nrow(input) - 1)) {
  for (j in (i + 1):nrow(input)) {
    max_area <- max(max_area, area(input[i, ], input[j, ]))
  }
}
print(max_area)