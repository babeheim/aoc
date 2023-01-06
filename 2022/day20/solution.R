
library(tictoc)

options(scipen=999) # always keep it off

move_element <- function(x, loc, n_moves) {
  if (n_moves < 0) {
    mv <- x[loc]
    x <- x[-loc]
    x <- rev(x)
    loc <- (length(x) + 1) - loc
    n_moves <- n_moves * (-1)
    new <- (loc + n_moves - 1) %% length(x) + 1
    x <- append(x, mv, new)
    x <- rev(x)
  } else {
    mv <- x[loc]
    x <- x[-loc]
    new <- (loc + n_moves - 1) %% length(x)
    x <- append(x, mv, new)
  }
  return(x)
}

mix_numbers <- function(path, multiplier = 1, n_times = 1) {
  dat <- data.frame(n_moves = as.numeric(readLines(path)))
  dat$n_moves <- dat$n_moves * multiplier
  n <- nrow(dat)
  ind <- 1:n
  dat$order <- ind
  for (j in seq_len(n_times)) {
    for (i in seq_len(n)) {
      src <- which(dat$order == i)
      dat <- dat[move_element(ind, src, dat$n_moves[src]),]
    }
  }
  out <- sum(dat$n_moves[(which(dat$n_moves == 0) + c(1000, 2000, 3000) - 1) %% n + 1])
  return(out)
}


stopifnot(mix_numbers("day20/test_input.txt") == 3)
tic("Day 20, part 1")
stopifnot(mix_numbers("day20/input.txt") == 2203)
toc(log=TRUE)

stopifnot(mix_numbers("day20/test_input.txt", multiplier = 811589153, n_times = 10) == 1623178306)
tic("Day 20, part 2")
stopifnot(mix_numbers("day20/input.txt", multiplier = 811589153, n_times = 10) == 6641234038999)
toc(log=TRUE)

