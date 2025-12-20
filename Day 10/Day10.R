require(R.utils, stringr)

input_file <- "sample.txt"
input <- readLines(input_file) |> strsplit(split = " ")

machines <- list()

for (i in seq_along(input)) {
  n <- nchar(input[[i]][1]) - 2
  l <- which((input[[i]][1] |> strsplit(split = ""))[[1]] == "#")
  b <- input[[i]][-c(1, length(input[[i]]))]
  j <- input[[i]][length(input[[i]])]
  machines[[i]] <- list("n" = n, "lights" = l - 2, "buttons" = b, "joltage" = j)
}

n <- machines[[1]]$n
verts <- intToBin(0:((2^n) - 1)) |> str_pad(n, side = "left", pad = "0")