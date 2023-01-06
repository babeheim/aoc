
library(tictoc)
library(dplyr)

count_calories <- function(path, n_elves = 1) {
  x <- readLines(path)
  x <- as.numeric(x)
  breaks <- which(is.na(x))
  starts <- c(1, breaks+1)
  stops <- c(breaks-1, length(x))

  for (i in seq_along(starts)) {
    add <- data.frame(
      elf = i,
      calories = x[starts[i]:stops[i]]
    )
    if (i == 1) {
      dat <- add
    } else {
      dat <- bind_rows(dat, add)
    }
  }

  dat |> group_by(elf) |> summarize(total_calories = sum(calories)) |> as.data.frame() |> arrange(desc(total_calories)) -> elves

  return(sum(elves$total_calories[1:n_elves]))
}


stopifnot(count_calories("day01/test_input.txt") == 24000)

tic("Day 01, part 1")
stopifnot(count_calories("day01/input.txt") == 67633)
toc(log = TRUE)


stopifnot(count_calories("day01/test_input.txt", n_elves = 3) == 45000)

tic("Day 01, part 2")
stopifnot(count_calories("day01/input.txt", n_elves 
= 3) == 199628)
toc(log = TRUE)
