
library(tictoc)

calc_score <- function(path, buggy = TRUE) {

  x <- readLines(path)

  if (buggy) {

    shape_score <- case_when(
      grepl("X", x) ~ 1,
      grepl("Y", x) ~ 2,
      grepl("Z", x) ~ 3,
    )

    wins <- c("A Y", "B Z", "C X")
    draws <- c("A X", "B Y", "C Z")
    loses <- c("A Z", "B X", "C Y")

    outcome_score <- case_when(
      x %in% wins ~ 6,
      x %in% draws ~ 3,
      x %in% loses ~ 0
    )

  } else {

    outcome_score <- case_when(
      grepl("X", x) ~ 0,
      grepl("Y", x) ~ 3,
      grepl("Z", x) ~ 6,
    )

    rock <- c("B X", "A Y", "C Z")
    paper <- c("C X", "B Y", "A Z")
    scissors <- c("A X", "C Y", "B Z")

    shape_score <- case_when(
      x %in% rock ~ 1,
      x %in% paper ~ 2,
      x %in% scissors ~ 3
    )

  }

  return(sum(shape_score + outcome_score))

}


stopifnot(calc_score("day02/test_input.txt") == 15)
stopifnot(calc_score("day02/input.txt") == 8392)

tic("Day 02, part 1")
stopifnot(calc_score("day02/test_input.txt", buggy = FALSE) == 12)
toc(log=TRUE)

tic("Day 02, part 2")
stopifnot(calc_score("day02/input.txt", buggy = FALSE) == 10116)
toc(log=TRUE)
