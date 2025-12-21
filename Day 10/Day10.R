require(R.utils, stringr, dplyr, igraph)

input_file <- "input.txt"
input <- readLines(input_file) |> strsplit(split = " ")

machines <- list()

for (i in seq_along(input)) {
  n <- nchar(input[[i]][1]) - 2
  l <- input[[i]][1] %>%
    gsub("#", "1", .) %>%
    gsub("\\.", "0", .) %>%
    gsub("[][]", "", .)
  b <- input[[i]][-c(1, length(input[[i]]))]
  j <- input[[i]][length(input[[i]])]
  machines[[i]] <- list("n" = n, "lights" = l, "buttons" = b, "joltage" = j)
}

graph_maker <- function(m) {
  n <- machines[[m]]$n
  verts <- intToBin(0:((2^n) - 1)) |>
    str_pad(n, side = "left", pad = "0") |>
    strsplit("")
  adj_mat <- matrix(0, nrow = 2^n, ncol = 2^n)
  b <- machines[[m]]$buttons %>% gsub("\\D", "", .)
  for (i in seq_along(b)) {
    inds <- (b[i] |> strsplit(split = ""))[[1]] |> as.numeric()
    inds <- (inds + 1) |> as.integer()
    for (j in 1:(length(verts) - 1)) {
      j_config <- grep("1", verts[[j]])
      for (k in (j + 1):length(verts)) {
        k_config <- grep("1", verts[[k]])
        if (identical(inds, sort(symdiff(j_config, k_config)))) {
          adj_mat[j, k] <- 1
          adj_mat[k, j] <- 1
        }
      }
    }
  }
  list(adj_mat, verts)
}

d <- 0
for (i in seq_along(machines)) {
  gm <- graph_maker(i)
  g <- gm[[1]] |> graph_from_adjacency_matrix()
  verts <- gm[[2]]
  endpoint <- machines[[i]]$lights |>
    strsplit("") |>
    unlist() %>%
    sapply(verts, identical, .) |>
    which(TRUE)
  d <- d + distances(g, 1, endpoint)
}
print(d)