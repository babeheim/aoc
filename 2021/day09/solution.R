
is_matrix_min <- function(x) {
  # calculate discrete gradients to find the minimum
  grad_right <- cbind((x[,2:ncol(x)] - x[,1:(ncol(x) - 1)]), rep(99, nrow(x)))
  grad_left <- cbind(rep(99, nrow(x)), (x[,1:(ncol(x) - 1)] - x[,2:ncol(x)]))
  is_local_min_x  <- grad_right > 0 & grad_left > 0
  grad_down <- rbind((x[2:nrow(x),] - x[1:(nrow(x) - 1),]), rep(99, ncol(x)))
  grad_up <- rbind(rep(99, ncol(x)), (x[1:(nrow(x) - 1),] - x[2:nrow(x),]))
  is_local_min_y  <- grad_down > 0 & grad_up > 0
  is_local_min <- is_local_min_x & is_local_min_y
  return(is_local_min)
}

copy_neighbor <- function(them, me) {
  if (is.na(me)) {
    if (!is.na(them) & them > 0) {
      out <- them
    } else {
      out <- me
    }
  } else {
    out <- me
  }
  return(out)
}

map_basins <- function(x) {
  # much simpler method using a while loop
  bsn <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  bsn[which(is_matrix_min(x))] <- 1:sum(is_matrix_min(x))
  bsn[which(x == 9)] <- 0 # all borders are group 0 (for now)
  while (any(is.na(bsn))) {
    for (k in 1:ncol(bsn)) {
      for (j in 1:nrow(bsn)) {
        if (is.na(bsn[j, k])) {
          if (j > 1)         bsn[j, k] <- copy_neighbor(bsn[j - 1, k], bsn[j, k])
          if (j < nrow(bsn)) bsn[j, k] <- copy_neighbor(bsn[j + 1, k], bsn[j, k])
          if (k > 1)         bsn[j, k] <- copy_neighbor(bsn[j, k - 1], bsn[j, k])
          if (k < ncol(bsn)) bsn[j, k] <- copy_neighbor(bsn[j, k + 1], bsn[j, k])
        }
      }
    }
  }
  bsn[bsn == 0] <- NA # set all borders to NA
  return(bsn)
}

calc_risk_level <- function(path) {
  raw <- readLines(path)
  x <- matrix(NA, ncol = nchar(raw[1]), nrow = length(raw))
  for (i in 1:nrow(x)) x[i,] <- as.numeric(strsplit(raw[i], split = "")[[1]])
  risk_levels <- x[which(is_matrix_min(x))] + 1
  sum(risk_levels)
}

calc_basin_score <- function(path) {
  raw <- readLines(path)
  x <- matrix(NA, ncol = nchar(raw[1]), nrow = length(raw))
  for (i in 1:nrow(x)) x[i,] <- as.numeric(strsplit(raw[i], split = "")[[1]])
  basin_matrix <- map_basins(x)
  out <- prod(rev(sort(table(basin_matrix)))[1:3])
  return(out)
}

# for part one
stopifnot(calc_risk_level("day09/test_input.txt") == 15)

tic("day 09, part 1")
stopifnot(calc_risk_level("day09/input.txt") == 514)
toc(log = TRUE)

# for part two
stopifnot(calc_basin_score("day09/test_input.txt") == 1134)

tic("day 09, part 2")
stopifnot(calc_basin_score("day09/input.txt") == 1103130)
toc(log = TRUE)
