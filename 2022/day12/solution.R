
library(tictoc)
library(dplyr)

vis_map <- function(path, highlight_lowest = FALSE) {

  x <- readLines(path)

  map <- matrix(NA, ncol = nchar(x[1]), nrow = length(x))

  start_x <- NA
  start_y <- NA
  stop_x <- NA
  stop_y <- NA

  for (i in 1:length(x)) {
    for (j in 1:nchar(x[1])) {
      elevation <- substr(x[i], j, j)
      if (elevation == "S") {
        elevation <- "a"
        start_x <- j
        start_y <- i
      }
      if (elevation == "E") {
        elevation <- "z"
        stop_x <- j
        stop_y <- i
      }
      map[i, j] <- match(elevation, letters)
    }
  }

  colors <- terrain.colors(26)
  if (highlight_lowest) colors[1] <- "#045200"

  plot(NULL, xlim = c(1 - 0.5, ncol(map) + 0.5), ylim = c(1 - 0.5, nrow(map) + 0.5), frame.plot = FALSE, axes = FALSE, xlab = "", ylab = "")
  for (i in 1:ncol(map)) {
    for (j in 1:nrow(map)) {
      rect(i - 0.5, j - 0.5, i + 0.5, j + 0.5, col = colors[map[j,i]], border = NA)
    }
  }
  points(start_x, start_y, col = "red", pch = 15, cex = 3)
  points(stop_x, stop_y, col = "red", pch = 1, cex = 3)

}

analyze_advent_map <- function(path, viz = FALSE, verbose = FALSE, teleportation = FALSE) {

  x <- readLines(path)

  map <- matrix(NA, ncol = nchar(x[1]), nrow = length(x))
  ind <- matrix(1:(nrow(map) * ncol(map)), nrow = nrow(map), ncol = ncol(map))

  start_char <- "S"
  stop_char <- "E"

  for (i in 1:length(x)) {
    for (j in 1:nchar(x[1])) {
      elevation <- substr(x[i], j, j)
      if (elevation == start_char) {
        start_x <- j
        start_y <- i
      }
      if (elevation == stop_char) {
        stop_x <- j
        stop_y <- i
      }
      if (elevation == "E") elevation <- "z"
      if (elevation == "S") elevation <- "a"
      map[i, j] <- match(elevation, letters)
    }
  }

  start_i <- ind[start_y, start_x]
  stop_i <- ind[stop_y, stop_x]

  edges <- data.frame(from = integer(), to = integer(), distance = integer())
  # build edge list
  for (i in 1:nrow(map)) {
    for (j in 1:ncol(map)) {
      # check north of current cell
      if (i > 1) {
        if (map[(i - 1), j] <= (map[i, j] + 1)) {
          add <- data.frame(from = ind[i, j], to = ind[i - 1, j], distance = 1L)
          edges <- bind_rows(edges, add)
        }
      }
      # check south of current cell
      if (i < nrow(map)) {
        if (map[(i + 1), j] <= (map[i, j] + 1)) {
          add <- data.frame(from = ind[i, j], to = ind[i + 1, j], distance = 1L)
          edges <- bind_rows(edges, add)
        }
      }
      # check east of current cell
      if (j > 1) {
        if (map[i, (j - 1)] <= (map[i, j] + 1)) {
          add <- data.frame(from = ind[i, j], to = ind[i, j - 1], distance = 1L)
          edges <- bind_rows(edges, add)
        }
      }
      # check west of current cell
      if (j < ncol(map)) {
        if (map[i, (j + 1)] <= (map[i, j] + 1)) {
          add <- data.frame(from = ind[i, j], to = ind[i, j + 1], distance = 1L)
          edges <- bind_rows(edges, add)
        }
      }
    }
  }

  if (teleportation) {
    # the second part is, what is the shortest path from ANY 'a' tile to the dest
    # reddit hint: just connect the start tile to all other 'a' tiles, with a distance of 0!
    other_a_tiles <- setdiff(ind[which(map == 1)], start_i)
    new_links <- expand.grid(from = start_i, to = other_a_tiles, distance = 0)
    edge_key <- paste(edges$from, edges$to)
    new_key <- paste(new_links$from, new_links$to)
    edges$distance[which(edge_key %in% new_key)] <- 0
    keep <- which(!(new_key %in% edge_key))
    if (length(keep) > 0) edges <- bind_rows(edges, new_links[keep,])
  }

  n_nodes <- length(unique(edges$from))
  nodes <- data.frame(
    visited = rep(FALSE, n_nodes),
    first_neighbor = rep(NA, n_nodes)
  )

  # add unvisited neighbors of the current node to a queue
  # process queue FIFO
  # when you find the dest node, you are done!

  queue <- start_i

  while (length(queue) > 0 & !all(nodes$visited) & !(stop_i %in% which(nodes$visited))) {
    focal <- queue[1]
    nodes$visited[focal] <- TRUE
    neighbors <- edges$to[which(edges$from == focal &
      edges$to %in% which(!nodes$visited) & !(edges$to %in% queue))]
    nodes$first_neighbor[neighbors] <- focal
    queue <- c(queue, neighbors)
    queue <- queue[-1]
  }
  
  shortest_path <- stop_i
  while (!is.na(nodes$first_neighbor[shortest_path[length(shortest_path)]])) {
    shortest_path <- c(shortest_path, nodes$first_neighbor[shortest_path[length(shortest_path)]])
    shortest_path
  }
  shortest_path <- rev(shortest_path)

  if (teleportation) shortest_path <- shortest_path[-1]
  # ignore the original start and use the first tile you 'teleported' to

  if (viz) {
    vis_map(path, highlight_lowest = TRUE)
    route <- data.frame(x = rep(NA, length(shortest_path)), y = rep(NA, length(shortest_path)))
    for (i in 1:length(shortest_path)) {
      route$y[i] <- (shortest_path[i] - 1) %% nrow(ind) + 1
      route$x[i] <- (shortest_path[i] - 1) %/% nrow(ind) + 1
    }
    points(route, type = "l", col = "black")
    points(route$x[1], route$y[1], pch = 20, cex = 2)
  }
  out <- (length(shortest_path) - 1) # minus one because we count the number of 'moves', so don't count the starting tile
  return(out)

}


stopifnot(analyze_advent_map("day12/test_input.txt") == 31)
tic("Day 12, part 1")
stopifnot(analyze_advent_map("day12/input.txt") == 484)
toc(log=TRUE)
# analyze_advent_map_dijkstra("day12/input.txt") == 484 # 15.649 sec

stopifnot(analyze_advent_map("day12/test_input.txt", teleportation = TRUE) == 29)

tic("Day 12, part 2")
stopifnot(analyze_advent_map("day12/input.txt", teleportation = TRUE) == 478)
toc(log=TRUE)
