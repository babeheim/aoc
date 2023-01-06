
# a brute-force DFS approach is not hard

library(dplyr)
library(tictoc)

compute_shortest_paths <- function(start, nodes, edges, verbose) {

  nodes$best_neighbor <- NA
  nodes$visited <- FALSE

  nodes$shortest_distance <- Inf
  nodes$shortest_distance[start] <- 0

  # While there are interesting nodes unvisited from the starting node...
  while (!all(nodes$visited) & any(!nodes$visited & nodes$shortest_distance < Inf)) {
    # ...find the unvisited node with the current shortest distance from starting node...
    focal <- which(!nodes$visited & nodes$shortest_distance == min(nodes$shortest_distance[which(!nodes$visited)]))[1]
    nodes$visited[focal] <- TRUE
    if (verbose) cat("Visiting node", focal, "with current shortest distance", nodes$shortest_distance[focal], "\n")
    # ...and for every unvisited node connnected to this focal node...
    neighbors <- edges$to[which(edges$from == focal & edges$to %in% which(!nodes$visited))]
    for (neighbor in neighbors) {
      # ...reroute through the focal if that's the shortest distance back to the start!
      if (nodes$shortest_distance[focal] + edges$distance[which(edges$from == focal & edges$to == neighbor)] < nodes$shortest_distance[neighbor]) {
        nodes$best_neighbor[neighbor] <- focal
        nodes$shortest_distance[neighbor] <- nodes$shortest_distance[focal] + edges$distance[which(edges$from == focal & edges$to == neighbor)]
        if (verbose) cat("Updating distance of node", neighbor, "to", nodes$shortest_distance[neighbor], "\n")
      }
      if (verbose) cat("Visited nodes:", which(nodes$visited), "\n")
      if (verbose) cat("Current shortest distances:",nodes$shortest_distance, "\n")
    }
  }

  return(nodes$shortest_distance)

}





calc_max_score <- function(path, part2 = FALSE) {

  x <- readLines(path)

  nodes <- data.frame(
    node = substr(x, 7, 8),
    rate = as.numeric(gsub("\\D", "", x))
  )

  edges <- data.frame(
    from = character(),
    to = character(),
    distance = integer()
  )

  for (i in 1:nrow(nodes)) {
    alters <- gsub("^.+; tunnel(s\\b|\\b) lead(s\\b|\\b) to valve(s\\b|\\b) ", "", x[i])
    alters <- strsplit(alters, ", ")[[1]]
    alters <- c(alters, nodes$node[i])
    for (j in 1:length(alters)) {
      add <- data.frame(from = nodes$node[i], to = alters[j])
      add$distance <- NA
      add$distance[which(add$from == add$to)] <- 0L
      add$distance[which(add$from != add$to)] <- 1L
      edges <- bind_rows(edges, add)
    }
  }

  edges$from <- match(edges$from, nodes$node)
  edges$to <- match(edges$to, nodes$node)

  dist <- matrix(NA, nrow = nrow(nodes), ncol = nrow(nodes))

  for (i in 1:nrow(dist)) {
    dist[,i] <- compute_shortest_paths(i, nodes, edges, verbose = FALSE)
  }

  magic_number <- 1340
  # this is the highest payoff for a single agent going for the time of 26 seconds; any route getting the second run has to already have a higher payoff

  if (part2) {
    time <- 26
  } else {
    time <- 30
  }
  # initial conditions of the simulation
  keep <- which(nodes$rate > 0 | nodes$node == "AA")
  nodes <- nodes[keep,,drop = FALSE]
  dist <- dist[keep,,drop = FALSE]
  dist <- dist[,keep,drop = FALSE]

  root <- list()
  root$payoff <- 0
  root$p1_time_used <- 0
  root$p1_time_left <- time - root$p1_time_used
  root$p1_location <- which(nodes$node == "AA")
  root$p1_visited <- root$p1_location
  root$p1_distances <- dist[,root$p1_location]
  root$p1_valuations <- nodes$rate * pmax((root$p1_time_left - root$p1_distances - 1), 0)
  root$p1_valuations[root$p1_visited] <- 0
  root$p2_time_used <- 0
  root$p2_time_left <- time - root$p2_time_used
  root$p2_location <- which(nodes$node == "AA")
  root$p2_visited <- root$p2_location
  root$p2_distances <- dist[,root$p2_location]
  root$p2_valuations <- nodes$rate * pmax((root$p2_time_left - root$p2_distances - 1), 0)
  root$p2_valuations[root$p2_visited] <- 0

  stack <- list(root)

  running_max <- 0

  verbose <- FALSE
  node_counter <- 1L

  while (length(stack) > 0) {

    if (verbose) {
      if (node_counter %% 1000L == 0) print(node_counter)
      node_counter <- node_counter + 1L
    }

    current <- stack[[length(stack)]]
    stack <- stack[-length(stack)]

    running_max <- max(running_max, current$payoff)

    if (current$p1_time_left > 0 & any(current$p1_valuations > 0)) {
      p1_targets <- which(current$p1_valuations > 0)
      for (j in seq_along(p1_targets)) {
        future <- current
        future$payoff <- current$payoff + current$p1_valuations[p1_targets[j]]
        future$p1_time_used <- current$p1_time_used + current$p1_distances[p1_targets[j]] + 1
        future$p1_time_left <- time - future$p1_time_used
        future$p1_location <- p1_targets[j]
        future$p1_visited <- c(current$p1_visited, p1_targets[j])
        future$p1_distances <- dist[,future$p1_location]
        future$p1_valuations <- nodes$rate * pmax((future$p1_time_left - future$p1_distances - 1), 0)
        future$p1_valuations[future$p1_visited] <- 0
        future$p2_valuations[future$p2_visited] <- 0
        stack[[length(stack)+1]] <- future
      }
    } else if (part2 & current$payoff >= magic_number & current$p2_time_left > 0 & any(current$p2_valuations > 0)) {
      p2_targets <- which(current$p2_valuations > 0)
      for (j in seq_along(p2_targets)) {
        future <- current
        future$payoff <- current$payoff + current$p2_valuations[p2_targets[j]]
        future$p2_time_used <- current$p2_time_used + current$p2_distances[p2_targets[j]] + 1
        future$p2_time_left <- time - future$p2_time_used
        future$p2_location <- p2_targets[j]
        future$p2_visited <- c(current$p2_visited, p2_targets[j])
        future$p2_distances <- dist[,future$p2_location]
        future$p2_valuations <- nodes$rate * pmax((future$p2_time_left - future$p2_distances - 1), 0)
        future$p2_valuations[c(future$p1_visited, future$p2_visited)] <- 0
        stack[[length(stack)+1]] <- future
      }
    }
  }

  running_max

}

stopifnot(calc_max_score("day16/test_input.txt") == 1651)

tic("day 16, part 1")
stopifnot(calc_max_score("day16/input.txt") == 1792) # boo ya!
toc(log = TRUE)

tic("day 16, part 2")
stopifnot(calc_max_score("day16/input.txt", part2 = TRUE) == 2587) # boo ya!
toc(log = TRUE)

