
sim_dumbos <- function(path, n_steps) {
  raw <- readLines(path)
  x <- matrix(NA, ncol = nchar(raw[1]), nrow = length(raw))
  for (i in 1:nrow(x)) x[i,] <- as.numeric(strsplit(raw[i], split = "")[[1]])
  n_flashes <- rep(0, n_steps)
  for (step in 1:n_steps) {
    # increment energy by 1
    x <- x + 1
    while (any(x >= 10, na.rm = TRUE)) {
      # identify flashers
      fl <- which(x >= 10)
      # record flash & set state to NA
      n_flashes[step] <- n_flashes[step] + length(fl)
      x[fl] <- NA
      # cascade flash to neighbors, including diagonals
      for (i in 1:length(fl)) {
        fl_j <- ((fl[i] - 1) %% nrow(x)) + 1
        fl_k <- ((fl[i] - 1) %/% nrow(x)) + 1
        north_min <- ifelse(fl_j > 1, fl_j - 1, fl_j)
        south_min <- ifelse(fl_j < nrow(x), fl_j + 1, fl_j)
        east_min <- ifelse(fl_k > 1, fl_k - 1, fl_k)
        west_min <- ifelse(fl_k < ncol(x), fl_k + 1, fl_k)
        x[north_min:south_min, east_min:west_min] <- x[north_min:south_min, east_min:west_min] + 1
      }
    }
    # set flashers' energy to 0
    x[which(is.na(x))] <- 0
  }
  return(n_flashes)
}

n_flashes <- sim_dumbos("day11/test_input.txt", 195)
stopifnot(sum(n_flashes[1:100]) == 1656) # part 1 test

stopifnot(min(which(n_flashes == 100)) == 195) # part 2 test

tic("day 11, part 1 & 2")
n_flashes <- sim_dumbos("day11/input.txt", 1000)
toc(log = TRUE)

stopifnot(sum(n_flashes[1:100]) == 1649) # part 1

stopifnot(min(which(n_flashes == 100)) == 256) # part 2
