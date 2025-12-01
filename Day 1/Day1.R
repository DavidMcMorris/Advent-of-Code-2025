library(dplyr)

input_filename <- "input.txt"
input <- read.table(input_filename)[[1]]
input <- gsub("L", "-", input) %>% gsub("R", "+", .) %>% as.numeric()

counter <- 0
value <- 50
for (i in seq_along(input)) {
  value <- (value + input[i]) %% 100
  if (value == 0) {
    counter <- counter + 1
  }
}

print(counter)