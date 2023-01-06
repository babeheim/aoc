
library(tictoc)

find_packet_start <- function(path, chunk_length = 4) {
  x <- readLines(path)
  n_chunks <- nchar(x) - (chunk_length - 1)
  dat <- matrix(NA, ncol = chunk_length, nrow = n_chunks)
  for (i in seq_len(nrow(dat))) {
    dat[i,] <- strsplit(substr(x, i, i + (chunk_length - 1)), "")[[1]]
  }
  chunk_is_unique <- apply(dat, 1, function(z) !any(duplicated(z)))
  out <- (chunk_length - 1) + min(which(chunk_is_unique))
  return(out)
}


stopifnot(find_packet_start("day06/test_input_1.txt") == 7)
stopifnot(find_packet_start("day06/test_input_2.txt") == 5)
stopifnot(find_packet_start("day06/test_input_3.txt") == 6)
stopifnot(find_packet_start("day06/test_input_4.txt") == 10)
stopifnot(find_packet_start("day06/test_input_5.txt") == 11)

tic("Day 06, part 1")
stopifnot(find_packet_start("day06/input.txt") == 1361)
toc(log=TRUE)

stopifnot(find_packet_start("day06/test_input_1.txt", chunk_length = 14) == 19)
stopifnot(find_packet_start("day06/test_input_2.txt", chunk_length = 14) == 23)
stopifnot(find_packet_start("day06/test_input_3.txt", chunk_length = 14) == 23)
stopifnot(find_packet_start("day06/test_input_4.txt", chunk_length = 14) == 29)
stopifnot(find_packet_start("day06/test_input_5.txt", chunk_length = 14) == 26)

tic("Day 06, part 2")
stopifnot(find_packet_start("day06/input.txt", chunk_length = 14) == 3263)
toc(log=TRUE)
