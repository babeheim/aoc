

text_to_vectors <- function(path) {
  x <- readLines(path)
  sub_direction <- gsub("\\s.*$", "", x)
  stopifnot(all(sub_direction %in% c("forward", "down", "up")))
  sub_magnitude <- as.numeric(gsub("^.*\\s", "", x))
  stopifnot(all(!is.na(sub_magnitude)))
  deltas <- matrix(0, ncol = 2, nrow = length(x)) # forward deltas, vertical deltas
  for (i in 1:nrow(deltas)) {
    if (sub_direction[i] == "forward") deltas[i, 1] <- sub_magnitude[i]
    # here 'up' and 'down' refer to vertical deltas
    if (sub_direction[i] == "up") deltas[i, 2] <- (-1) * sub_magnitude[i]
    if (sub_direction[i] == "down") deltas[i, 2] <- sub_magnitude[i]
  }
  positions <- matrix(NA, ncol = 2, nrow = (nrow(deltas) + 1)) # horizonal position, vertical position
  positions[1,] <- c(0, 0)
  # horizonal position as cumulative horizonal deltas
  positions[2:nrow(positions), 1] <- cumsum(deltas[, 1])
  # vertical position as cumulative vertical deltas
  positions[2:nrow(positions), 2] <- cumsum(deltas[, 2])
  out <- prod(positions[nrow(positions),1:2])
  return(out)
}

text_to_vectors_with_aim <- function(path) {
  x <- readLines(path)
  sub_direction <- gsub("\\s.*$", "", x)
  stopifnot(all(sub_direction %in% c("forward", "down", "up")))
  sub_magnitude <- as.numeric(gsub("^.*\\s", "", x))
  stopifnot(all(!is.na(sub_magnitude)))
  deltas <- matrix(0, ncol = 3, nrow = length(x)) # forward deltas, vertical deltas, aim deltas
  for (i in 1:nrow(deltas)) {
    if (sub_direction[i] == "forward") deltas[i, 1] <- sub_magnitude[i]
    # here 'up' and 'down' refer to aim deltas, not vertical deltas
    if (sub_direction[i] == "up") deltas[i, 3] <- (-1) * sub_magnitude[i]
    if (sub_direction[i] == "down") deltas[i, 3] <- sub_magnitude[i]
  }
  positions <- matrix(NA, ncol = 3, nrow = (nrow(deltas) + 1)) # horizonal position, vertical position, cumulative aim
  positions[1,] <- c(0, 0, 0)
  # horizonal position as cumulative horizonal deltas
  positions[2:nrow(positions), 1] <- cumsum(deltas[, 1])
  # aim as cumulative aim deltas
  positions[2:nrow(positions), 3] <- cumsum(deltas[, 3])
  # vertical delta = horizontal delta * current (cumulative) aim
  deltas[, 2] <- deltas[, 1] * positions[2:nrow(positions), 3]
  # vertical position as cumulative vertical deltas
  positions[2:nrow(positions), 2] <- cumsum(deltas[, 2])
  out <- prod(positions[nrow(positions),1:2])
  return(out)
}

stopifnot(text_to_vectors("day02/test_input.txt") == 150)

tic("day 02, part 1")
stopifnot(text_to_vectors("day02/input.txt") == 1762050)
toc(log = TRUE)

stopifnot(text_to_vectors_with_aim("day02/test_input.txt") == 900)

tic("day 02, part 2")
stopifnot(text_to_vectors_with_aim("day02/input.txt") == 1855892637)
toc(log = TRUE)
