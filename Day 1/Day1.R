library(dplyr)

input_file <- "input.txt"
input <- read.table(input_file)[[1]]
input <- gsub("L", "-", input) %>% gsub("R", "+", .) %>% as.numeric()

extra <- 0
counter <- c(0, 0)
value <- 50
for (i in seq_along(input)) {
  turns <- abs(input[i]) %/% 100
  spin <- sign(input[i]) * (abs(input[i]) %% 100)
  new_value <- (value + spin) %% 100
  if (new_value == 0) {
    counter <- counter + c(1, 1)
  } else if ((sign(spin) * (new_value - value)) < 0 && value != 0) {
    counter <- counter + c(0, 1)
  }
  value <- new_value
  extra <- extra + turns
}
print(counter + c(0, extra))