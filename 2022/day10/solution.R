
library(tictoc)
library(dplyr)

calc_signal_strength <- function(path) {

  x <- readLines(path)
  ops <- data.frame(
    line = x
  )

  ops$cycles_used <- case_when(
    grepl("noop", ops$line) ~ 0L,
    grepl("addx", ops$line) ~ 1L
  )

  ops$delta_X <- NA
  ops$delta_X[which(ops$line == "noop")] <- 0L
  tar <- grep("addx", ops$line)
  ops$delta_X[tar] <- as.integer(gsub("addx\\s", "", ops$line[tar]))

  ops$cycle_starting <- NA
  ops$cycle_ending <- NA

  for (i in seq_len(nrow(ops))) {
    if (i == 1) {
      # initalization conditions
      ops$cycle_starting[i] <- 1L
    } else {
      # each command begins on the cycle *after* the last command ends, so we add 1 here
      ops$cycle_starting[i] <- ops$cycle_ending[i-1] + 1L
    }
    ops$cycle_ending[i] <- ops$cycle_starting[i] + ops$cycles_used[i]
  }
  
  n_cycles <- ops$cycle_ending[nrow(ops)]

  cycles <- data.frame(
    cycle = seq_len(n_cycles),
    X_starting = NA,
    X_ending = NA
  )

  for (j in seq_len(nrow(cycles))) {
    tar <- which(ops$cycle_ending == j)
    if (length(tar) > 0) {
      net_change <- ops$delta_X[tar]
    } else {
      net_change <- 0L
    }
    if (j == 1) {
      # inital value of X is 1
      cycles$X_starting[j] <- 1L
    } else {
      cycles$X_starting[j] <- cycles$X_ending[j-1]
    }
    cycles$X_ending[j] <- cycles$X_starting[j] + net_change
  }

  # interesting signal strengths found 'during the cycle' for target cycles
  # 'during the cycle' means before the cycle ends, so X_starting
  if (n_cycles >= 60) {
    targets <- c(20, 20 + seq_len((n_cycles - 20) %/% 40) * 40)
    out <- sum(targets * cycles$X_starting[targets])
  } else {
    out <- 0
  }
  return(out)
}

draw_screen <- function(path) {

  x <- readLines(path)
  ops <- data.frame(
    line = x
  )

  ops$cycles_used <- case_when(
    grepl("noop", ops$line) ~ 0L,
    grepl("addx", ops$line) ~ 1L
  )

  ops$delta_X <- NA
  ops$delta_X[which(ops$line == "noop")] <- 0L
  tar <- grep("addx", ops$line)
  ops$delta_X[tar] <- as.integer(gsub("addx\\s", "", ops$line[tar]))

  ops$cycle_starting <- NA
  ops$cycle_ending <- NA

  for (i in seq_len(nrow(ops))) {
    if (i == 1) {
      # initalization conditions
      ops$cycle_starting[i] <- 1L
    } else {
      # each command begins on the cycle *after* the last command ends, so we add 1 here
      ops$cycle_starting[i] <- ops$cycle_ending[i-1] + 1L
    }
    ops$cycle_ending[i] <- ops$cycle_starting[i] + ops$cycles_used[i]
  }
  
  n_cycles <- ops$cycle_ending[nrow(ops)]

  cycles <- data.frame(
    cycle = seq_len(n_cycles),
    X_starting = NA,
    X_ending = NA
  )

  for (j in seq_len(nrow(cycles))) {
    tar <- which(ops$cycle_ending == j)
    if (length(tar) > 0) {
      net_change <- ops$delta_X[tar]
    } else {
      net_change <- 0L
    }
    if (j == 1) {
      # inital value of X is 1
      cycles$X_starting[j] <- 1L
    } else {
      cycles$X_starting[j] <- cycles$X_ending[j-1]
    }
    cycles$X_ending[j] <- cycles$X_starting[j] + net_change
  }

  cycles$draw_pixel <- (cycles$cycle - 1) %% 40
  cycles$sprite_position <- cycles$X_starting
  
  cycles <- select(cycles, draw_pixel, sprite_position)

  screen <- matrix(NA, ncol = 40, nrow = 6)

  cycles$hit <- NA
  for (i in 1:nrow(cycles)) {
    if (cycles$draw_pixel[i] %in% (c(-1, 0, 1) + cycles$sprite_position[i])) {
      cycles$hit[i] <- "#"
    } else {
      cycles$hit[i] <- "."
    }
  }

  grid <- matrix(cycles$hit, byrow = TRUE, ncol = 40, nrow = 6)
  out <- character()
  for (i in 1:6) out[i] <- paste(grid[i,], collapse = "")
  return(out)

}


stopifnot(calc_signal_strength("day10/test_input.txt") == 0)
stopifnot(calc_signal_strength("day10/test_input_ii.txt") == 13140)
tic("Day 10, part 1")
stopifnot(calc_signal_strength("day10/input.txt") == 14420)
toc(log=TRUE)

draw_screen("day10/test_input_ii.txt")
tic("Day 10, part 2")
correct_answer <- c("###...##..#....###..###..####..##..#..#.", "#..#.#..#.#....#..#.#..#....#.#..#.#..#.", 
"#..#.#....#....#..#.###....#..#..#.#..#.", "###..#.##.#....###..#..#..#...####.#..#.", 
"#.#..#..#.#....#.#..#..#.#....#..#.#..#.", "#..#..###.####.#..#.###..####.#..#..##.."
)
stopifnot(all(draw_screen("day10/input.txt") == correct_answer))
# RGLRBZAU
cat("Day 10, part 2 needs an interpreter\n")
toc(log=TRUE)
