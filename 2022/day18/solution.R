
library(tictoc)
library(dplyr)

calc_surface_area2 <- function(path, outer_only = FALSE) {
  x <- readLines(path)
  dat <- matrix(NA, nrow = length(x), ncol = 3)
  for (i in 1:nrow(dat)) dat[i,] <- as.integer(strsplit(x[i], split = ",")[[1]])

  # reindex at 1
  dat[,1] <- dat[,1] - min(dat[,1]) + 1L
  dat[,2] <- dat[,2] - min(dat[,2]) + 1L
  dat[,3] <- dat[,3] - min(dat[,3]) + 1L

  # enclose array in a box of 0's
  n_x <- max(dat[,1]) + 2L
  n_y <- max(dat[,2]) + 2L
  n_z <- max(dat[,3]) + 2L
  dat[,1] <- dat[,1] + 1L
  dat[,2] <- dat[,2] + 1L
  dat[,3] <- dat[,3] + 1L

  ar <- array(0L, dim = c(n_x, n_y, n_z))

  for (i in 1:nrow(dat)) {
    ar[dat[i,1], dat[i,2], dat[i,3]] <- 1L
  }

  if (outer_only) {
    fq <- data.frame(x = 1, y = 1, z = 1, visited = FALSE)
    # for each unvisited member of focal queue, find all unvisited air neighbors and add them to focal queue
    while (!all(fq$visited)) {
      focus <- min(which(!fq$visited))
      if (fq$x[focus] < dim(ar)[1] && ar[fq$x[focus] + 1L, fq$y[focus], fq$z[focus]] == 0L) {
        add <- data.frame(x = fq$x[focus] + 1L, y = fq$y[focus], z = fq$z[focus], visited = FALSE)
        if (!(any(fq$x == add$x & fq$y == add$y & fq$z == add$z))) fq <- bind_rows(fq, add)
      }
      if (fq$x[focus] > 1L && ar[fq$x[focus] - 1L, fq$y[focus], fq$z[focus]] == 0L) {
        add <- data.frame(x = fq$x[focus] - 1L, y = fq$y[focus], z = fq$z[focus], visited = FALSE)
        if (!(any(fq$x == add$x & fq$y == add$y & fq$z == add$z))) fq <- bind_rows(fq, add)
      }
      if (fq$y[focus] < dim(ar)[2] && ar[fq$x[focus], fq$y[focus] + 1L, fq$z[focus]] == 0L) {
        add <- data.frame(x = fq$x[focus], y = fq$y[focus] + 1L, z = fq$z[focus], visited = FALSE)
        if (!(any(fq$x == add$x & fq$y == add$y & fq$z == add$z))) fq <- bind_rows(fq, add)
      }
      if (fq$y[focus] > 1L && ar[fq$x[focus], fq$y[focus] - 1L, fq$z[focus]] == 0L) {
        add <- data.frame(x = fq$x[focus], y = fq$y[focus] - 1L, z = fq$z[focus], visited = FALSE)
        if (!(any(fq$x == add$x & fq$y == add$y & fq$z == add$z))) fq <- bind_rows(fq, add)
      }
      if (fq$z[focus] < dim(ar)[3] && ar[fq$x[focus], fq$y[focus], fq$z[focus] + 1L] == 0L) {
        add <- data.frame(x = fq$x[focus], y = fq$y[focus], z = fq$z[focus] + 1L, visited = FALSE)
        if (!(any(fq$x == add$x & fq$y == add$y & fq$z == add$z))) fq <- bind_rows(fq, add)
      }
      if (fq$z[focus] > 1L && ar[fq$x[focus], fq$y[focus], fq$z[focus] - 1L] == 0L) {
        add <- data.frame(x = fq$x[focus], y = fq$y[focus], z = fq$z[focus] - 1L, visited = FALSE)
        if (!(any(fq$x == add$x & fq$y == add$y & fq$z == add$z))) fq <- bind_rows(fq, add)
      }
      fq$visited[focus] <- TRUE
    }

    ext <- array(0L, dim = c(n_x, n_y, n_z))
    for (i in 1:dim(ar)[1]) {
      for (j in 1:dim(ar)[2]) {
        for (k in 1:dim(ar)[3]) {
          if (any(fq$x == i & fq$y == j & fq$z == k)) {
            ext[i,j,k] <- 1L
          }
        }
      }
    }

  }
  

  ad <- array(0L, dim = c(n_x, n_y, n_z))

  for (i in 1:dim(ar)[1]) {
    for (j in 1:dim(ar)[2]) {
      for (k in 1:dim(ar)[3]) {
        # if it is an air voxel...
        if (ar[i, j, k] == 0L) {
          # count up how many of the four neighboring voxels are lava
          faces <- 0L
          if (i < dim(ar)[1] && ar[i + 1, j, k] == 1L) faces <- faces + 1L
          if (i > 1          && ar[i - 1, j, k] == 1L) faces <- faces + 1L
          if (j < dim(ar)[2] && ar[i, j + 1, k] == 1L) faces <- faces + 1L
          if (j > 1          && ar[i, j - 1, k] == 1L) faces <- faces + 1L
          if (k < dim(ar)[3] && ar[i, j, k + 1] == 1L) faces <- faces + 1L
          if (k > 1          && ar[i, j, k - 1] == 1L) faces <- faces + 1L
          if (outer_only && ext[i, j, k] == 0L) faces <- 0L
          ad[i, j, k] <- faces
        }
      }
    }
  }

  out <- sum(ad)
  return(out)

}

stopifnot(calc_surface_area2("day18/test_input.txt") == 64)
stopifnot(calc_surface_area2("day18/test_input_ii.txt") == 10)
stopifnot(calc_surface_area2("day18/test_input_iii.txt") == 4332)

tic("Day 18, part 1")
stopifnot(calc_surface_area2("day18/input.txt") == 4364)
toc(log = TRUE)

stopifnot(calc_surface_area2("day18/test_input.txt", outer_only = TRUE) == 58)

tic("Day 18, part 2")
stopifnot(calc_surface_area2("day18/input.txt", outer_only = TRUE) == 2508)
toc(log = TRUE)
