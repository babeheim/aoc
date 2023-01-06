
sim_cucumbers <- function(file, verbose = FALSE) {
  A_raw <- readLines(file)
  A <- matrix(NA, ncol = nchar(A_raw[1]), nrow = length(A_raw))
  for (row in seq_len(length(A_raw))) A[row,] <- strsplit(A_raw[row], "")[[1]]

  turn <- 0
  right_works_i <- 1 # dummy values
  down_works_i <- 1 # dummy values

  while(length(right_works_i) > 0 | length(down_works_i) > 0) {

    turn <- turn + 1

    A_propose_right <- A[, c(ncol(A), 1:(ncol(A)-1))]
    right_works_i <- which(A == "." & A_propose_right == ">")
    if (length(right_works_i) > 0) {
      right_works_j <- (right_works_i - 1) %% nrow(A) + 1
      right_works_k <- (right_works_i - 1) %/% nrow(A) + 1
      right_from_k <- right_works_k - 1L
      if (any(right_from_k == 0L)) right_from_k[which(right_from_k == 0L)] <- ncol(A)
      for (i in seq_along(right_works_i)) {
        A[right_works_j[i], right_works_k[i]] <- ">"
        A[right_works_j[i], right_from_k[i]] <- "."
      }
      if (verbose) print(paste("shifted", length(right_works_i), "right"))
    }

    A_propose_down <- A[c(nrow(A), 1:(nrow(A) - 1)), ]
    down_works_i <- which(A == "." & A_propose_down == "v")
    if (length(down_works_i) > 0) {
      down_works_j <- (down_works_i - 1) %% nrow(A) + 1
      down_works_k <- (down_works_i - 1) %/% nrow(A) + 1
      down_from_j <- down_works_j - 1L
      if (any(down_from_j == 0L)) down_from_j[which(down_from_j == 0L)] <- nrow(A)
      for (i in seq_along(down_works_i)) {
        A[down_works_j[i], down_works_k[i]] <- "v"
        A[down_from_j[i], down_works_k[i]] <- "."
      }
      if (verbose) print(paste("shifted", length(down_works_j), "down"))
    }

    if (verbose) print(paste("turn", turn, "complete"))

  }
  return(turn)
}

stopifnot(sim_cucumbers("day25/test_input.txt") == 58)
tic("day 25, part 1")
stopifnot(sim_cucumbers("day25/input.txt") == 563)
toc(log = TRUE)
