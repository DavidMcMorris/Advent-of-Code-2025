library(igraph)
input_file <- "input.txt"
input <- read.csv(input_file, header = FALSE)

distances <- dist(input, method = "euclidean") |> as.matrix()
dims <- dim(distances)
links <- matrix(1, nrow = dims[1], ncol = dims[2])

i <- 0
flag <- 0
while (flag == 0) {
  unlinked <- distances * links
  ind <- which(unlinked == min(unlinked[unlinked != 0]))
  if (length(ind) > 1) {
    links[ind] <- 0
  }
  c <- abs(links - 1) |> graph_from_adjacency_matrix() |> count_components()
  if (c == 1) {
    join <- input[arrayInd(ind[1], dims)[1, ], ]
    prod(join[, 1]) |> print()
    flag <- 1
  }
  i <- i + 1
  if (i == 1000) {
    x <- abs(links - 1) |> graph_from_adjacency_matrix() |> components()
    x <- x$csize |> sort() |> rev()
    prod(x[1:3]) |> print()
  }
}