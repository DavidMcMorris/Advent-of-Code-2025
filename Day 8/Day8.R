input_file <- "sample.txt"
input <- read.csv(input_file, header = FALSE)

distances <- dist(input, method = "euclidean") |> as.matrix()
dims <- dim(distances)
links <- matrix(0, nrow = dims[1], ncol = dims[2])