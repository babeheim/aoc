
options(scipen = 999)

# second approach, using a vector of nine state bins
sim_lanternfish <- function(path, n_days = 80, verbose = FALSE) {
  # initialize counter vector
  n <- rep(0, 9) # number in state 0 is n[0 + 1], in state 8 is n[8 + 1]
  # load initial population
  init <- as.numeric(strsplit(readLines(path), split = ",")[[1]])
  for (i in (0:8 + 1)) {
    n[i] <- sum(init == (i - 1))
  }
  if (verbose) print(paste("before day 1 there are", sum(n), "lanternfish"))
  # experience each day
  for (i in 1:n_days) {
    n[7 + 1] <- n[7 + 1] + n[0 + 1] # parents switch from state 0 to state 7
    n <- c(n[1:8 + 1], n[0 + 1]) # all decrease 1 state; n[0 + 1] new offspring appear in state 8
    if (verbose) if (i %% 10 == 0) print(paste("after day", i, "there are", sum(n), "lanternfish"))
  }
  if (verbose) print(paste("after day", i, "there are", sum(n), "lanternfish"))
  return(sum(n))
}


stopifnot(sim_lanternfish("day06/test_input.txt", n_days = 18) == 26)

stopifnot(sim_lanternfish("day06/test_input.txt") == 5934)

# in part 1, we run for 80 days
tic("day 06, part 1")
stopifnot(sim_lanternfish("day06/input.txt") == 385391)
toc(log = TRUE)

stopifnot(sim_lanternfish("day06/test_input.txt", n_days = 256) == 26984457539)

tic("day 06, part 2")
stopifnot(sim_lanternfish("day06/input.txt", n_days = 256) == 1728611055389)
toc(log = TRUE)