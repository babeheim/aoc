
library(tictoc)

number_scraper <- function(pattern, x) {
  m <- regexpr(pattern, x, perl = TRUE)
  as.numeric(regmatches(x, m))
}

count_overlaps <- function(path, type = "subsets") {
  x <- readLines(path)
  A_start <- number_scraper("^\\d+", x)
  A_stop <- number_scraper("\\d+(?=\\,)", x)
  B_start <- number_scraper("(?<=,)\\d+", x)
  B_stop <- number_scraper("\\d+$", x)
  if (type == "subsets") {
    out <- sum(
      A_start <= B_start & A_stop >= B_stop |
      B_start <= A_start & B_stop >= A_stop)
  } else if (type == "all") {
    n_non_overlaps <- sum(
      B_start > A_stop |
      B_stop < A_start
    )
    # so overlaps are everything else
    out <- length(A_start) - n_non_overlaps
  } else {
    stop("invalid type argument")
  }
  return(out)
}

stopifnot(count_overlaps("day04/test_input.txt") == 2)

tic("Day 04, part 1")
stopifnot(count_overlaps("day04/input.txt") == 542)
toc(log=TRUE)

stopifnot(count_overlaps("day04/test_input.txt", type = "all") == 4)

tic("Day 04, part 2")
stopifnot(count_overlaps("day04/input.txt", type = "all") == 900)
toc(log=TRUE)

