
read_sum_diff <- function(path) {
  x <- as.numeric(readLines(path))
  sum(diff(x) > 0)
}

read_sum_diff_three <- function(path) {
  x <- as.numeric(readLines(path))
  x_sum <- rep(NA, (length(x) - 2))
  for (i in 1:(length(x) - 2)) x_sum[i] <- x[i] + x[i + 1] + x[i + 2]
  sum(diff(x_sum) > 0)
}

# part 1
stopifnot(read_sum_diff("day01/test_input.txt") == 7)
tic("day 01, part 1")
stopifnot(read_sum_diff("day01/input.txt") == 1316)
toc(log = TRUE)

# part 2
stopifnot(read_sum_diff_three("day01/test_input.txt") == 5)
tic("day 01, part 2")
stopifnot(read_sum_diff_three("day01/input.txt") == 1344)
toc(log = TRUE)
