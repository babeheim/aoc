
library(tictoc)

# Cartesian distance
dist <- function(focal, parent) {
  sqrt((focal$x - parent$x)^2 + (focal$y - parent$y)^2)
}

update_node <- function(focal, parent) {
  parent_dist <- dist(focal, parent)
  if (parent_dist == 2) {
    # parent cardinal 2 away
    if (parent$x > focal$x & parent$y == focal$y) {
      focal$x <- focal$x + 1L
    } else if (parent$x < focal$x & parent$y == focal$y) {
      focal$x <- focal$x - 1L
    } else if (parent$y > focal$y & parent$x == focal$x) {
      focal$y <- focal$y + 1L
    } else if (parent$y < focal$y & parent$x == focal$x) {
      focal$y <- focal$y - 1L
    } else stop("should not happen 1")
  } else if (parent_dist == sqrt(5)) {
    # parent is a knight's move away
    if ((parent$x - focal$x) > 1L) {
      focal$x <- focal$x + 1L
      focal$y <- parent$y
    } else if ((parent$y - focal$y) > 1L) {
      focal$y <- focal$y + 1L
      focal$x <- parent$x
    } else if ((parent$x - focal$x) < -1L) {
      focal$x <- focal$x - 1L
      focal$y <- parent$y
    } else if ((parent$y - focal$y) < -1L) {
      focal$y <- focal$y - 1L
      focal$x <- parent$x
    } else stop("should not happen 2")
  } else if (parent_dist == sqrt(8)) {
    # parent is two diagonals away
    if (parent$y > focal$y) {
      focal$y <- focal$y + 1L
    } else {
      focal$y <- focal$y - 1L
    }
    if (parent$x > focal$x) {
      focal$x <- focal$x + 1L
    } else {
      focal$x <- focal$x - 1L
    }
  } else if (parent_dist < 2) {
    # nothing!
  } else stop ("should not happen")
  return(focal)
}

prep_input <- function(x) {
  out <- character(0)
  for (i in seq_along(x)) {
    move <- gsub("[^A-Z]", "", x[i])
    n_times <- as.numeric(gsub("\\D", "", x[i]))
    out <- c(out, rep(move, n_times))
  }
  return(out)
}

trace_tail <- function(path, rope_length = 2) {

  x <- readLines(path)
  commands <- prep_input(x)

  rope <- data.frame(
    x = rep(0L, rope_length),
    y = rep(0L, rope_length)
  )

  tail <- data.frame(x = 0L, y = 0L)

  for (j in seq_along(commands)) {
    if (commands[j] == "R") {
      rope$x[1] <- rope$x[1] + 1L
    }
    if (commands[j] == "L") {
      rope$x[1] <- rope$x[1] - 1L
    }
    if (commands[j] == "U") {
      rope$y[1] <- rope$y[1] + 1L
    }
    if (commands[j] == "D") {
      rope$y[1] <- rope$y[1] - 1L
    }
    for (i in 2:rope_length) {
      rope[i,] <- update_node(rope[i,], rope[(i-1),])
    }
    stopifnot(all(abs(diff(rope$x)) <= 1L))
    stopifnot(all(abs(diff(rope$y)) <= 1L))
    tail <- dplyr::bind_rows(tail, rope[rope_length,])
  }

  out <- nrow(unique(tail))
  return(out)

}

stopifnot(trace_tail("day09/test_input.txt") == 13)

tic("Day 09, part 1")
stopifnot(trace_tail("day09/input.txt") == 5883)
toc(log=TRUE)

stopifnot(trace_tail("day09/test_input.txt", rope_length = 10) == 1)
stopifnot(trace_tail("day09/test_input_part2.txt", rope_length = 10) == 36)

tic("Day 09, part 2")
stopifnot(trace_tail("day09/input.txt", rope_length = 10) == 2367)
toc(log=TRUE)
