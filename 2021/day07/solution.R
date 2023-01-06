
total_cost <- function(path, value = NULL, verbose = FALSE) {
  x <- as.numeric(strsplit(readLines(path), ",")[[1]])
  if (is.null(value)) value <- median(x)
  abs_dev <- abs(x - value)
  total_absolute_dev <- sum(abs_dev)
  if (verbose) print(paste("position", value, "has total fuel cost", total_absolute_dev))
  return(total_absolute_dev)
}

total_cost_cumulative <- function(path, value = NULL, verbose = FALSE) {
  if (length(value) < 2) {
    x <- as.numeric(strsplit(readLines(path), ",")[[1]])
    if (is.null(value)) value <- round(mean(x))
    abs_dev <- abs(x - value)
    total_cumulative_abs_dev <- sum(abs_dev * (abs_dev + 1) / 2)
    if (verbose) print(paste("position", value, "has total cumulative fuel cost", total_cumulative_abs_dev))
    return(total_cumulative_abs_dev)
  } else {
    sapply(value, function(z) total_cost_cumulative(path, z))
  }
}

stopifnot(total_cost("day07/test_input.txt", 1) == 41)
stopifnot(total_cost("day07/test_input.txt", 3) == 39)
stopifnot(total_cost("day07/test_input.txt", 10) == 71)
stopifnot(total_cost("day07/test_input.txt") == 37)

tic("day 07, part 1")
stopifnot(total_cost("day07/input.txt") == 340987)
toc(log = TRUE)

