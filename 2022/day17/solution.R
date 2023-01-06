
library(tictoc)

spawn_piece <- function(current_piece) {
  if (current_piece == "—") {
    piece_mat <- cbind(
      c(0:3),
      rep(0L, 4)
    )
  } else if (current_piece == "+" ) {
    piece_mat <- cbind(
      c(1L, 0L, 1L, 2L, 1L),
      c(0L, 1L, 1L, 1L, 2L)
    )
  } else if (current_piece == "J" ) {
    piece_mat <- cbind(
      c(0L, 1L, 2L, 2L, 2L),
      c(0L, 0L, 0L, 1L, 2L)
    )
  } else if (current_piece == "I" ) {
    piece_mat <- cbind(
      c(0L, 0L, 0L, 0L),
      c(0L, 1L, 2L, 3L)
    )
  } else if (current_piece == "O" ) {
    piece_mat <- cbind(
      c(0L, 1L, 1L, 0L),
      c(0L, 0L, 1L, 1L)
    )
  } else {
    stop("invalid piece")
  }
  return(piece_mat)
}

check_collision <- function(piece, arena) {
  hit <- rep(NA, nrow(piece))
  for (i in 1:nrow(piece)) {
    if (piece[i,1] > 7L | piece[i,1] < 1L) {
      hit[i] <- TRUE
    } else {
      hit[i] <- arena[piece[i,2], piece[i,1]] == 1L
    } 
  }
  return(hit)
}

sim_tetris <- function(path, n_pieces = 2022) {

  jet_sequence <- strsplit(readLines(path), split = "")[[1]]
  jet_sequence <- ifelse(jet_sequence == ">", 1L, -1L)

  jet_seqlen <- length(jet_sequence)

  piece_list <- c("—", "+", "J", "I", "O")

  # inital parameter values
  arena_floor <- 0L # initalize this at 0 for scrolling
  current_floor <- 1L # start at level 1 for the floor
  jet_pointer <- 1L
  pieces_dropped <- 0L

  # initialize the arena
  ar <- matrix(0L, ncol = 7, nrow = 100)
  ar[current_floor,] <- 1L

  while (pieces_dropped < n_pieces) {

    current_piece <- piece_list[pieces_dropped %% 5L + 1L]

    # define the new spawn point
    sp_y <- current_floor + 4L - arena_floor # 3 empty lines separate
    sp_x <- 3L

    # spawn a piece
    piece_mat <- spawn_piece(current_piece)
    # locate in the arena
    piece_mat[, 1] <- piece_mat[, 1] + sp_x
    piece_mat[, 2] <- piece_mat[, 2] + sp_y

    falling <- TRUE
    hit_detected <- FALSE

    while (falling) {

      stopifnot(piece_mat[,1] <= 7L)
      stopifnot(piece_mat[,1] >= 1L)

      # apply the next push in the jet_sequence
      candidate_piece_mat <- piece_mat
      candidate_piece_mat[,1] <- candidate_piece_mat[,1] + jet_sequence[jet_pointer]

      hit <- check_collision(candidate_piece_mat, ar)
      if (!any(hit)) piece_mat <- candidate_piece_mat
      jet_pointer <- jet_pointer + 1L
      if (jet_pointer == (jet_seqlen + 1L)) jet_pointer <- 1L

      # vertical collision check
      candidate_piece_mat <- piece_mat
      candidate_piece_mat[,2] <- candidate_piece_mat[,2] - 1L
      hit <- check_collision(candidate_piece_mat, ar)
      if (!any(hit)) {
        piece_mat <- candidate_piece_mat
      } else {
        falling <- FALSE
        for (i in 1:nrow(piece_mat)) {
          ar[piece_mat[i,2], piece_mat[i,1]] <- 1L
        }
        current_floor <- max(which(apply(ar == 1, 1, any))) + arena_floor
      }

    }

    pieces_dropped <- pieces_dropped + 1L
    # if (pieces_dropped %% 1000 == 0) cat(pieces_dropped, "\n")
    
    if ((nrow(ar) - (current_floor - arena_floor)) < 10) {
#     cat("approaching end of arena\n")
      ar <- rbind(ar[51:nrow(ar),], matrix(0L, nrow = 50, ncol = 7))
      arena_floor <- arena_floor + 50L
    }

  }

  out <- current_floor - 1
  return(out)

}

stopifnot(sim_tetris("day17/test_input.txt") == 3068)

tic("Day 17, part 1")
stopifnot(sim_tetris("day17/input.txt") == 3098)
toc(log = TRUE)

cat("Day 17, part 2 is calculated by hand\n")

