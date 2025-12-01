library(dplyr)

input_file <- "input.txt"
input <- read.table(input_file)[[1]]
input <- gsub("L", "-", input) %>% gsub("R", "+", .) %>% as.numeric()

counter <- c(0, 0)
value <- 50
for (i in seq_along(input)) {
  counter <- counter + c(0, abs(input[i]) %/% 100)
  new_value <- (value + input[i]) %% 100
  if (new_value == 0) {
    counter <- counter + c(1, 1)
  } else if ((sign(input[i]) * (new_value - value)) < 0 && value != 0) {
    counter <- counter + c(0, 1)
  }
  value <- new_value
}
print(counter)