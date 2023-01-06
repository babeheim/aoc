

# arithmetic using the 100-sided 'deterministic dice'
m <- function(i) 3 * (3 * i - 1) # moves rolled on turn i
s <- function(i) (3/2) * i * (3 * i + 1) # total number of moves up to and including turn i
v <- function(i) {
  # total number of moves across even turns up to and including turn i
  if (i %% 2 == 0) {
    (3/2) * i * ((3/2) * i + 2)
  } else {
    v(i - 1)
  }
}
d <- function(i) {
  # total number of moves across odd turns up to and including turn i
  if (i %% 2 != 0) {
    (3/2) * (i + 1) * (3 * i + 1)/2
  } else {
    d(i - 1)
  }
}
p1_score_turn <- function(t, i = 4, m = 10) {
  if (t %% 2 != 0) {
    return((i + d(t) - 1) %% m + 1)
  } else {
    return(0)
  }
}
p2_score_turn <- function(t, i = 8, m = 10) {
  if (t %% 2 == 0) {
    return((i + v(t) - 1) %% m + 1)
  } else {
    return(0)
  }
}

play_deterministic_game <- function(p1_init, p2_init) {
  turns <- 2:1000
  p1_total_score <- p2_total_score <- rep(NA, length(turns) + 1)
  p1_total_score[1] <- p1_score_turn(1, i = p1_init)
  p2_total_score[1] <- 0
  for (turn in turns) {
    p1_total_score[turn] <- p1_total_score[turn - 1] + p1_score_turn(turn, i = p1_init)
    p2_total_score[turn] <- p2_total_score[turn - 1] + p2_score_turn(turn, i = p2_init)
  }
  i_win <- min(which(p1_total_score >= 1000 | p2_total_score >= 1000))
  out <- list(
    i_win = i_win,
    p1_total_score = p1_total_score[i_win],
    p2_total_score = p2_total_score[i_win],
    target = i_win * 3 * min(c(p1_total_score[i_win], p2_total_score[i_win]))
  )
}

# test data:
res <- play_deterministic_game(4, 8)
stopifnot(res$i_win == 331)
stopifnot(res$p1_total_score == 1000)
stopifnot(res$p2_total_score == 745)
stopifnot(res$target == 739785)

# real data:
tic("day 21, part 1")
res <- play_deterministic_game(1, 2)
stopifnot(res$i_win == 364)
stopifnot(res$p1_total_score == 548)
stopifnot(res$p2_total_score == 1007)
stopifnot(res$target == 598416)
toc(log = TRUE)

play_stochastic_game <- function(p1_pos, p2_pos, verbose = FALSE) {

  p1_pos <- as.integer(p1_pos)
  p2_pos <- as.integer(p2_pos)

  n_turns <- 30
  winning_score <- 21

  # initialize an empty array to store timeline counts for each turn
  A <- array(c(0L, 0L, 0L, 0L, 0L), dim = c(10, winning_score, 10, winning_score, n_turns + 1))

  # initialize 'turn 0' before the first roll
  p1_score <- 0L
  p2_score <- 0L
  turn <- 0
  A[p1_pos, p1_score + 1, p2_pos, p2_score + 1, turn + 1] <- 1
  if (verbose) print(paste("before first turn, player 1 on", p1_pos, "with score", p1_score, "and player 2 on", p2_pos, "with score", p2_score))

  new_winners <- rep(0L, n_turns)

  for (turn in seq_len(n_turns)) {
    if (verbose) print(paste("playing turn", turn))
    for (roll_one in 1:3) {
      for (roll_two in 1:3) {
        for (roll_three in 1:3) {
          roll <- roll_one + roll_two + roll_three
          if (verbose) print(paste("turn", turn, "rolled", roll))
          for (p1_last_pos in 1:10) {
            for (p1_last_score in 0:(winning_score - 1)) {
              for (p2_last_pos in 1:10) {
                for (p2_last_score in 0:(winning_score - 1)) {
                  if (turn %% 2L != 0) {
                    p1_pos <- (p1_last_pos + roll - 1L) %% 10L + 1L
                    p1_score <- p1_last_score + p1_pos
                    p2_pos <- p2_last_pos
                    p2_score <- p2_last_score
                    if (p1_score < winning_score) {
                      if (A[p1_last_pos, p1_last_score + 1, p2_last_pos, p2_last_score + 1, (turn + 1) - 1] > 0L) {
                        if (verbose) print(paste("after turn", turn, ", player 1 on", p1_pos, "with score", p1_score))
                        A[p1_pos, p1_score + 1, p2_pos, p2_score + 1, turn + 1] <- A[p1_pos, p1_score + 1, p2_pos, p2_score + 1, turn + 1] + A[p1_last_pos, p1_last_score + 1, p2_last_pos, p2_last_score + 1, (turn + 1) - 1]
                      }
                    } else {
                      if (A[p1_last_pos, p1_last_score + 1, p2_last_pos, p2_last_score + 1, (turn + 1) - 1] > 0L) {
                        if (verbose) print(paste("after turn", turn, ", player 1 wins with score", p1_score))
                        new_winners[turn] <- new_winners[turn] + A[p1_last_pos, p1_last_score + 1, p2_last_pos, p2_last_score + 1, (turn + 1) - 1]
                      }
                    }
                  } else {
                    p1_pos <- p1_last_pos
                    p1_score <- p1_last_score
                    p2_pos <- (p2_last_pos + roll - 1L) %% 10L + 1L
                    p2_score <- p2_last_score + p2_pos
                    if (p2_score < winning_score) {
                      if (A[p1_last_pos, p1_last_score + 1, p2_last_pos, p2_last_score + 1, (turn + 1) - 1] > 0L) {
                        if (verbose) print(paste("after turn", turn, ", player 2 on", p2_pos, "with score", p2_score))
                        A[p1_pos, p1_score + 1, p2_pos, p2_score + 1, turn + 1] <- A[p1_pos, p1_score + 1, p2_pos, p2_score + 1, turn + 1] + A[p1_last_pos, p1_last_score + 1, p2_last_pos, p2_last_score + 1, (turn + 1) - 1]
                      }
                    } else {
                      if (A[p1_last_pos, p1_last_score + 1, p2_last_pos, p2_last_score + 1, (turn + 1) - 1] > 0L) {
                        if (verbose) print(paste("after turn", turn, ", player 2 wins with score", p2_score))
                        new_winners[turn] <- new_winners[turn] + A[p1_last_pos, p1_last_score + 1, p2_last_pos, p2_last_score + 1, (turn + 1) - 1]
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    if (sum(A[ , , , , turn + 1]) == 0L) break()
  }
  out <- list(
    p1_winners = sum(new_winners[seq(1, n_turns, 2)]),
    p2_winners = sum(new_winners[seq(2, n_turns, 2)])
  )
}

# test input
res <- play_stochastic_game(4, 8)
stopifnot(res$p1_winners == 444356092776315)
stopifnot(res$p2_winners == 341960390180808)
# 57% chance of p1 winning

# real input
tic("day 21, part 2")
res <- play_stochastic_game(1, 2)
toc(log = TRUE)
stopifnot(res$p1_winners == 27674034218179)
stopifnot(res$p2_winners == 17242469745088)
# 62% chance of p1 winning
