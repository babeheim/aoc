
find_grid_neighbors <- function(index, n, m) {
  index_j <- (index - 1) %% n + 1
  index_k <- (index - 1) %/% n + 1
  neighbors_j <- index_j + c(0, 0, -1, 1)
  neighbors_k <- index_k + c(-1, 1, 0, 0)
  neighbors_i <- neighbors_j + (neighbors_k - 1) * n
  drop <- which(neighbors_j < 1 | neighbors_j > n | neighbors_k < 1 | neighbors_k > m)
  if (length(drop) > 0) neighbors_i <- neighbors_i[-drop]
  return(neighbors_i)
}

calc_least_cost_path <- function(file, use_full = FALSE, verbose = FALSE, store_path = FALSE) {
  # load cost data
  raw <- readLines(file)
  n <- length(raw)
  m <- nchar(raw[1])
  cost <- matrix(NA, nrow = n, ncol = m)
  for (i in 1:length(raw)) cost[i,] <- as.numeric(strsplit(raw[i], "")[[1]])
  if (use_full) {
    # construct full map, 5x5 = 25 tiles
    # each tile increments all values by +1 down and +1 right
    cost_full <- matrix(NA, nrow = n * 5, ncol = m * 5)
    register_map <- c(1:9, 1:8) # highest value is 9 + 8 -> 8
    for (k in 1:5) {
      for (j in 1:5) {
        cost_to_add <- cost + (j - 1) + (k - 1)
        cost_to_add <- matrix(register_map[cost_to_add], nrow = n, ncol = m)
        cost_full[(j - 1)*n + 1:n, (k - 1)*m + 1:m] <- cost_to_add
      }
    }
    cost <- cost_full
    n <- nrow(cost)
    m <- ncol(cost)
  }
  # initialize algorithm components
  min_dist <- rep(Inf, times = n*m)
  min_dist[1] <- 0
  visited <- rep(FALSE, times = n*m)
  if (store_path) path <- vector("list", n*m)
  # run search loop
  while (!all(visited)) {
    # ...find the unvisited node with the current shortest distance from starting node...
    focal <- which(!visited & min_dist == min(min_dist[!visited]))[1]
    if (verbose) cat("Visiting node", focal, "with current shortest distance", min_dist[focal], "\n")
    # identify neighbors in the grid in north-south, east-west relationships only
    neighbors_i <- find_grid_neighbors(focal, n, m)
    for (neighbor in neighbors_i) {
      if (!visited[neighbor]) {
        if (verbose) print(paste("evaluating neighbor", neighbor, "of current focal", focal))
        if (cost[neighbor] + min_dist[focal] < min_dist[neighbor]) {
          min_dist[neighbor] <- cost[neighbor] + min_dist[focal]
          if (verbose) print(paste("shortest path to", neighbor, "now", min_dist[neighbor], "via patch", focal))
          if (store_path) path[[neighbor]] <- c(path[[focal]], focal)
        } else {
          if (verbose) print (paste("path to", neighbor, "unchanged"))
        }
      }
    }
    visited[focal] <- TRUE
  }
  return(min_dist[n*m])
}

# part one:
stopifnot(calc_least_cost_path("day15/test_input.txt") == 40) # 0.02 sec

tic("day 15, part 1")
stopifnot(calc_least_cost_path("day15/input.txt") == 583) # 0.9 sec
toc(log = TRUE)

# part two:
stopifnot(calc_least_cost_path("day15/test_input.txt", use_full = TRUE) == 315) # 0.226 sec

tic("day 15, part 2")
stopifnot(calc_least_cost_path("day15/input.txt", use_full = TRUE) == 2927) # 11 minutes
toc(log = TRUE)
