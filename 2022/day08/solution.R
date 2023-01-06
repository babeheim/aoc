
library(tictoc)

running_max <- function(x) c(-1, cummax(x[1:(length(x)-1)]))

count_visible_trees <- function(path) {
  x <- paste(readLines(path), collapse = "")
  x <- as.numeric(strsplit(x, "")[[1]])
  n_side <- sqrt(length(x))
  dat <- matrix(x, ncol = n_side, byrow = TRUE)

  max_east <- t(apply(dat, 1, running_max))
  max_south <- apply(dat[nrow(dat):1,], 2, running_max)[nrow(dat):1,]
  max_north <- apply(dat, 2, running_max)
  max_west <- t(apply(t(dat)[nrow(dat):1,], 2, running_max)[nrow(dat):1,])

  # lets count the hidden ones rather than the visible ones
  n_hidden <- sum(dat <= max_east &
    dat <= max_west &
    dat <= max_north &
    dat <= max_south)

  n_visible <- n_side^2 - n_hidden
  out <- n_visible
  return(out)
}

calc_scenic_score <- function(x, y, dat) {
  
  n_side <- nrow(dat) 

  if (y > 1) {
    north_view <- dat[(y-1):1, x]
    north_score <- min(c(which(north_view >= dat[y,x]), length(north_view)))
  } else {
    north_score <- 0
  }

  if (y < n_side) {
    south_view <- dat[(y+1):n_side, x]
    south_score <- min(c(which(south_view >= dat[y,x]), length(south_view)))
  } else {
    south_score <- 0
  }

  if (x > 1) {
    east_view <- dat[y, (x-1):1]
    east_score <- min(c(which(east_view >= dat[y,x]), length(east_view)))
  } else {
    east_score <- 0
  }

  if (x < n_side) {
    west_view <- dat[y, (x+1):n_side]
    west_score <- min(c(which(west_view >= dat[y,x]), length(west_view)))
  } else {
    west_score <- 0
  }

  out <- north_score * south_score * east_score * west_score
  return(out)  

}

calc_max_scenic_score <- function(path) {
  x <- paste(readLines(path), collapse = "")
  x <- as.numeric(strsplit(x, "")[[1]])
  n_side <- sqrt(length(x))
  dat <- matrix(x, ncol = n_side, byrow = TRUE)
  out <- matrix(NA, ncol = n_side, nrow = n_side)
  for (i in seq_len(n_side)) {
    for (j in seq_len(n_side)) {
      out[i, j] <- calc_scenic_score(i, j, dat)
    }
  }
  return(max(out))
}


# part 1 
stopifnot(count_visible_trees("day08/test_input.txt") == 21)

tic("Day 08, part 1")
stopifnot(count_visible_trees("day08/input.txt") == 1705)
toc(log=TRUE)

# part 2
stopifnot(calc_max_scenic_score("day08/test_input.txt") == 8)

tic("Day 08, part 2")
stopifnot(calc_max_scenic_score("day08/input.txt") == 371200)
toc(log=TRUE)
