
library(tictoc)
options(scipen=999) # always keep it off

calc_monkey_number <- function(path) {
  # path <- "day21/test_input.txt"
  x <- readLines(path)
  x <- gsub("(?<=[a-z]) ", "() ", x, perl = TRUE)
  x <- gsub("(?<=[a-z])$", "()", x, perl = TRUE)
  x <- gsub(": ", " <- function() ", x)
  for (i in 1:length(x)) {
    eval(parse(text = x[i]))
  }
  root()
}


calc_monkey_number_part2 <- function(path, my_input) {
  x <- readLines(path)
  x <- gsub("(?<=[a-z]) ", "() ", x, perl = TRUE)
  x <- gsub("(?<=[a-z])$", "()", x, perl = TRUE)
  x <- gsub(": ", " <- function() ", x)
  x[grep("^humn", x)] <- "humn <- function(input = my_input) input"
  root_row <- grep("^root", x)
  x[root_row] <- gsub("\\+", "==", x[root_row]) 
  for (i in 1:length(x)) {
    eval(parse(text = x[i]))
  }
  root()
}

stopifnot(calc_monkey_number("day21/test_input.txt") == 152)

tic("Day 21, part 1")
stopifnot(calc_monkey_number("day21/input.txt") == 160274622817992)
toc(log=TRUE)

stopifnot(calc_monkey_number_part2("day21/test_input.txt", 301) == TRUE)
tic("Day 21, part 2")
stopifnot(calc_monkey_number_part2("day21/input.txt", 3087390115721) == TRUE)
toc(log=TRUE)


