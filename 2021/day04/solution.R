
library(tictoc)

score_board <- function(board, draws) {
  unmarked_sum <- sum(as.numeric(board[!(board %in% draws)]))
  unmarked_sum * draws[length(draws)]
}

read_bingo <- function(path) {
  raw <- readLines(path)
  out <- list()
  out$draws <- as.numeric(strsplit(raw[1], split = ",")[[1]])
  board_starts <- which(raw == "") + 1
  board_starts <- setdiff(board_starts, length(raw) + 1) # just in case it was picking up last line
  out$board_array <- array(NA, dim = c(5, 5, length(board_starts)))
  for (i in 1:length(board_starts)) {
    for (j in 1:5) {
      raw_line <- raw[board_starts[i] + (j - 1)]
      raw_line <- gsub("^\\s+", "", raw_line)
      out$board_array[j,,i] <- as.numeric(strsplit(raw_line, split = "\\s+")[[1]])
    }
  }
  return(out)
}

find_bingos <- function(bingo_dat) {
  past_winners <- integer()
  for (i in 1:length(bingo_dat$draws)) {
    hit_array <- array(bingo_dat$board_array %in% bingo_dat$draws[1:i], dim = dim(bingo_dat$board_array))
    row_sums <- apply(hit_array, c(1, 3), sum) # hit sum for row i, board j
    col_sums <- apply(hit_array, c(2, 3), sum) # hit sum for column i, board j
    winner <- integer()
    if (any(row_sums == 5)) {
      winner <- c(winner, which(apply(row_sums == 5, 2, sum) > 0))
    }
    if (any(col_sums == 5)) {
      winner <- c(winner, which(apply(col_sums == 5, 2, sum) > 0))
    }
    winner <- sort(unique(winner))
    winner <- setdiff(winner, past_winners)
    past_winners <- c(past_winners, winner)
    if (length(winner) == 1) {
      print(paste("board", winner, "is a winner after draw", i))
    }
    if (length(winner) > 1) {
      winner_string <- paste(winner, collapse = ", ")
      print(paste("boards", winner_string, "are winners after draw", i))
    }
    if (length(winner) == dim(bingo_dat$board_array)[3]) break()
    if (length(past_winners) == 0 & i == length(bingo_dat$draws)) {
      print("no winner found after all draws!")
    }
  }
}

dat <- read_bingo("day04/test_input.txt")
find_bingos(dat) # "board 3 is a winner after draw 12"
stopifnot(score_board(dat$board_array[,,3], dat$draws[1:12]) == 4512)

tic("day 04, part 1")
dat <- read_bingo("day04/input.txt")
find_bingos(dat) # "board 17 is a winner after draw 22"
stopifnot(score_board(dat$board_array[,,17], dat$draws[1:22]) == 33462)
toc(log = TRUE)

# part 2 - find the last board that wins

dat <- read_bingo("day04/test_input.txt")
find_bingos(dat) # "board 2 is a winner after draw 15"
stopifnot(score_board(dat$board_array[,,2], dat$draws[1:15]) == 1924)

tic("day 04, part 2")
dat <- read_bingo("day04/input.txt")
find_bingos(dat) # "board 92 is a winner after draw 83"
stopifnot(score_board(dat$board_array[,,92], dat$draws[1:83]) == 30070)
toc()
