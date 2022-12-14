
# [Day 13](https://adventofcode.com/2022/day/13)

```R

# [[],[[],10,6,8]]

left <- fromJSON("[1,1,3,1,1]", simplifyVector = FALSE)
right <- fromJSON("[1,1,5,1,1]", simplifyVector = FALSE)

left[1][[1]] >= right[1][[1]]
left[2][[1]] >= right[2][[1]]
left[3][[1]] >= right[3][[1]]
left[4][[1]] >= right[4][[1]]
left[5][[1]] >= right[5][[1]]


list(
  list(),
  list(
    list(),
    10,
    6,
    8
  )
)

```



# [Day 12: Hill Climbing Algorithm](https://adventofcode.com/2022/day/12)

a is lowest, b then up to z
you can only go 1 letter up in elevation each step
"the elevation of the destination square can be much lower than the elevation of your current square" implies that you can go more than 1 letter down tho...

There's an S (the start) with elevantion a and an E (the goal) which has elevation z.

The goal is to do it in as few steps as possible. Sounds like Djikstra! The cost of movement determines which adjacent cells are available.

```R

rm(list = ls())

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

# this uses dijkstra's algorithm
compute_shortest_path <- function(start, dest, edges, verbose = TRUE) {

  n_nodes <- length(unique(edges$from))
  nodes <- data.frame(
    shortest_distance = rep(Inf, n_nodes),
    best_neighbor = rep(NA, n_nodes),
    visited = rep(FALSE, n_nodes)
  )
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

  shortest_path <- dest
  while (!is.na(nodes$best_neighbor[shortest_path[length(shortest_path)]])) {
    shortest_path <- c(shortest_path, nodes$best_neighbor[shortest_path[length(shortest_path)]])
    shortest_path
  }
  out <- rev(shortest_path)
  return(out)

}


analyze_advent_map_dijkstra <- function(path, viz = FALSE, verbose = FALSE, teleportation = FALSE) {

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
    # the second part is, what is the shortest path from ANY 'a' tile to the destination
    # reddit hint: just connect the start tile to all other 'a' tiles, with a distance of 0!
    other_a_tiles <- setdiff(ind[which(map == 1)], start_i)
    new_links <- expand.grid(from = start_i, to = other_a_tiles, distance = 0)
    edge_key <- paste(edges$from, edges$to)
    new_key <- paste(new_links$from, new_links$to)
    edges$distance[which(edge_key %in% new_key)] <- 0
    keep <- which(!(new_key %in% edge_key))
    if (length(keep) > 0) edges <- bind_rows(edges, new_links[keep,])
  }

  shortest_path <- compute_shortest_path(start_i, stop_i, edges, verbose)

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
    # the second part is, what is the shortest path from ANY 'a' tile to the destination
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
  # when you find the destination node, you are done!

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

analyze_advent_map("day12/test_input.txt") == 31
analyze_advent_map("day12/input.txt") == 484 # 14.463 sec
# analyze_advent_map_dijkstra("day12/input.txt") == 484 # 15.649 sec

analyze_advent_map("day12/test_input.txt", teleportation = TRUE) == 29
analyze_advent_map("day12/input.txt", teleportation = TRUE) == 478


```

In part 2, we have to find the shortest path across all the shortest paths from *every* elevation a. Reddit gave a great hint: just create new links between each of of the elevation 'a' tiles and the start with 0 distance, so they all effectively serve as logical starting places.



# [Day 11: Monkey in the Middle](https://adventofcode.com/2022/day/11)

We have to represent a graph that things move through.
A directed edge list, with the rule itself!
A monkey list, with the operations each does.
There's always an additional operation after the monkey's operation, the worry level is divided by 3 and rounded down to the nearest integer.
A *turn* is all the items in one monkey's possession, but the item goes to the next monkey it will also get inspected by that monkey.
A *round* is all the monkeys going for one turn.
We also have an *item* list. The items have a *current value* and a *current monkey*. Each time step, we apply (1) the transformation described by that monkey, (2) identify the *next monkey* based on the edge list, and then we replace the current monkey with the next monkey.

This could be a dirac dice situation? We shall see!

We count the total number of times a monkey inspects an item over 20 rounds, and find the two most active monkeys.

```R

rm(list = ls())

library(dplyr)



##############

update_worry <- function(worry, monkey, ops) {
  ops[[monkey]](worry)
}

test_item <- function(worry, monkey, mod = test_modulos) {
  worry %% mod[monkey] == 0
}

find_next_monkey <- function(current, test, links) {
  links$next_monkey[which(links$current_monkey == current & links$test_condition == test)]
}

# identify next_monkey for each item this round

simulate_transfers <- function(path, n_rounds = 20, use_modulo = TRUE) {

  x <- readLines(path)

  n_monkeys <- sum(grepl("Monkey \\d:", x))

  # create items table
  item_rows <- grep("  Starting items:", x)
  items <- data.frame(worry = numeric(), current_monkey = numeric())
  for (i in seq_along(item_rows)) {
    worry_scores <- gsub(".*: ", "", x[item_rows[i]])
    worry_scores <- as.numeric(strsplit(worry_scores, ", ")[[1]])
    add <- data.frame(worry = worry_scores, current_monkey = i)
    items <- bind_rows(items, add)
  }
  items$next_monkey <- NA

  # create links table
  links <- data.frame(current_monkey = numeric(), next_monkey = numeric(), test_condition = logical())
  link_rows <- grep("    If true: throw to monkey", x)
  for (i in seq_along(link_rows)) {
    add <- data.frame(
      current_monkey = i,
      # dont forget we index monkeys from 1
      next_monkey = as.numeric(gsub("\\D", "", x[link_rows[i]])) + 1,
      test_condition = TRUE
    )
    links <- bind_rows(links, add)
  }
  link_rows <- grep("    If false: throw to monkey", x)
  for (i in seq_along(link_rows)) {
    add <- data.frame(
      current_monkey = i,
      # dont forget we index monkeys from 1
      next_monkey = as.numeric(gsub("\\D", "", x[link_rows[i]])) + 1,
      test_condition = FALSE
    )
    links <- bind_rows(links, add)
  }
  links <- arrange(links, links$current_monkey)

  # extract test modulos
  mod_rows <- grep("  Test: divisible by ", x)
  test_modulos <- as.numeric(gsub("\\D", "", x[mod_rows]))

  supermodulo <- prod(test_modulos)

  # create operations list

  op_rows <- grep("  Operation:", x)
  if (use_modulo) {
    op_code <- paste0(gsub("  Operation: new = ", "function(old) floor((", x[op_rows]), ") / 3)")
  } else {
    op_code <- paste0(gsub("  Operation: new = ", "function(old) floor(", x[op_rows]), ")")
  }
  inspection_ops <- vector("list", length(op_code))
  for (i in seq_along(op_code)) inspection_ops[[i]] <- eval(parse(text = op_code[i]))


  # begin transfer simulation!

  counts <- data.frame(
    round = integer(),
    n_items = integer()
  )

  # loop over monkeys ("turns") then items within each monkey
  for (round in seq_len(n_rounds)) {
    for (m in seq_len(n_monkeys)) {
      my_items <- which(items$current_monkey == m)
      if (length(my_items) > 0) {
        for (i in my_items) {
          if (use_modulo) {
            items$worry[i] <- update_worry(items$worry[i], m, inspection_ops)
          } else {
            items$worry[i] <- update_worry(items$worry[i] %% supermodulo, m, inspection_ops)
          }
          items$test[i] <- test_item(items$worry[i], m, test_modulos)
          items$next_monkey[i] <- find_next_monkey(m, items$test[i], links)
        }
        # items immediately go to the next monkey
        items$current_monkey[my_items] <- items$next_monkey[my_items]
        items$next_monkey[my_items] <- NA
      }
      add <- data.frame(
        round = round,
        monkey = m,
        n_items = length(my_items)
      )
      counts <- bind_rows(counts, add)
    }
    if (round %% 100 == 0) print(round)
  }

  counts |>
    group_by(monkey) |>
    summarize(
      total_items = sum(n_items)
    ) |> arrange(desc(total_items)) -> totals

  out <- as.numeric(totals$total_items[1]) * as.numeric(totals$total_items[2])

  return(out)

}

simulate_transfers("day11/test_input.txt", n_rounds = 20) == 10605

simulate_transfers("day11/input.txt", n_rounds = 20) == 72884


simulate_transfers("day11/test_input.txt", n_rounds = 10000, use_modulo = FALSE) == 2713310158

simulate_transfers("day11/input.txt", n_rounds = 10000, use_modulo = FALSE) == 15310845153

```

Explanation of the supermodulo trick: https://www.reddit.com/r/adventofcode/comments/zih7gf/2022_day_11_part_2_what_does_it_mean_find_another/






# [Day 10: Cathode-Ray Tube](https://adventofcode.com/2022/day/10)

great viz: https://www.reddit.com/r/adventofcode/comments/zhmsg2/2022_day_10_sprites_and_not_the_elf_kind/

X starts at 1
`addx V` takes two cycles to complete; after two cycles, add `V` to the X register
`noop` takes one cycle to complete and has no effect

```R

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

calc_signal_strength("day10/test_input.txt") == 0
calc_signal_strength("day10/test_input_ii.txt") == 13140
calc_signal_strength("day10/input.txt") == 14420

draw_screen("day10/test_input_ii.txt")
draw_screen("day10/input.txt") # RGLRBZAU

```




# [Day 9: Rope Bridge](https://adventofcode.com/2022/day/9)

[Reddit Solution Megathread](https://www.reddit.com/r/adventofcode/comments/zgnice/2022_day_9_solutions/)

https://twitter.com/kjmimmack/status/1601398760239943681

https://github.com/norbertkehrer/aoc-2022

it looks like this uses [Chebyshev distance](https://en.wikipedia.org/wiki/Chebyshev_distance) aka "chessboard distance". Contrast with Manhatten distance.

look at this amazing viz: https://www.reddit.com/r/adventofcode/comments/zgq3nr/2022_day_9_rope_pull/

```R

# Cartesian distance
dist <- function(focal, parent) {
  sqrt((focal$x - parent$x)^2 + (focal$y - parent$y)^2)
}

update_node <- function(focal, parent) {
  parent_dist <- dist(focal, parent)
  if (parent_dist == 2) {
    # parent cardinal 2 away
    if (parent$x > focal$x & parent$y == focal$y) {
      focal$x <- focal$x + 1L
    } else if (parent$x < focal$x & parent$y == focal$y) {
      focal$x <- focal$x - 1L
    } else if (parent$y > focal$y & parent$x == focal$x) {
      focal$y <- focal$y + 1L
    } else if (parent$y < focal$y & parent$x == focal$x) {
      focal$y <- focal$y - 1L
    } else stop("should not happen 1")
  } else if (parent_dist == sqrt(5)) {
    # parent is a knight's move away
    if ((parent$x - focal$x) > 1L) {
      focal$x <- focal$x + 1L
      focal$y <- parent$y
    } else if ((parent$y - focal$y) > 1L) {
      focal$y <- focal$y + 1L
      focal$x <- parent$x
    } else if ((parent$x - focal$x) < -1L) {
      focal$x <- focal$x - 1L
      focal$y <- parent$y
    } else if ((parent$y - focal$y) < -1L) {
      focal$y <- focal$y - 1L
      focal$x <- parent$x
    } else stop("should not happen 2")
  } else if (parent_dist == sqrt(8)) {
    # parent is two diagonals away
    if (parent$y > focal$y) {
      focal$y <- focal$y + 1L
    } else {
      focal$y <- focal$y - 1L
    }
    if (parent$x > focal$x) {
      focal$x <- focal$x + 1L
    } else {
      focal$x <- focal$x - 1L
    }
  } else if (parent_dist < 2) {
    # nothing!
  } else stop ("should not happen")
  return(focal)
}

prep_input <- function(x) {
  out <- character(0)
  for (i in seq_along(x)) {
    move <- gsub("[^A-Z]", "", x[i])
    n_times <- as.numeric(gsub("\\D", "", x[i]))
    out <- c(out, rep(move, n_times))
  }
  return(out)
}

trace_tail <- function(path, rope_length = 2) {

  x <- readLines(path)
  commands <- prep_input(x)

  rope <- data.frame(
    x = rep(0L, rope_length),
    y = rep(0L, rope_length)
  )

  tail <- data.frame(x = 0L, y = 0L)

  for (j in seq_along(commands)) {
    if (commands[j] == "R") {
      rope$x[1] <- rope$x[1] + 1L
    }
    if (commands[j] == "L") {
      rope$x[1] <- rope$x[1] - 1L
    }
    if (commands[j] == "U") {
      rope$y[1] <- rope$y[1] + 1L
    }
    if (commands[j] == "D") {
      rope$y[1] <- rope$y[1] - 1L
    }
    for (i in 2:rope_length) {
      rope[i,] <- update_node(rope[i,], rope[(i-1),])
    }
    stopifnot(all(abs(diff(rope$x)) <= 1L))
    stopifnot(all(abs(diff(rope$y)) <= 1L))
    tail <- dplyr::bind_rows(tail, rope[rope_length,])
  }

  out <- nrow(unique(tail))
  return(out)

}

trace_tail("day09/test_input.txt") == 13
trace_tail("day09/test_input.txt", rope_length = 10) == 1

trace_tail("day09/test_input_part2.txt", rope_length = 10) == 36

trace_tail("day09/input.txt") == 5883
trace_tail("day09/input.txt", rope_length = 10) == 2367

```

# [Day 8: Treetop Tree House](https://adventofcode.com/2022/day/8)

[Reddit Solutions](https://www.reddit.com/r/adventofcode/comments/zfpnka/2022_day_8_solutions/)



```R

running_max <- function(x) c(-1, cummax(x[1:(length(x)-1)]))

count_visible_trees <- function(path) {
  x <- paste(readLines(path), collapse = "")
  x <- as.numeric(strsplit(x, "")[[1]])
  n_side <- sqrt(length(x))
  dat <- matrix(x, ncol = n_side, byrow = TRUE)

  max_east <- t(apply(dat, 1, running_max))
  max_south <- apply(dat[nrow(dat):1,], 2, running_max)[nrow(dat):1,]
  max_north <- apply(dat, 2, running_max)
  max_west <- t(apply(t(dat)[nrow(dat):1,], 2, running_max)[nrow(dat):1,])

  # lets count the hidden ones rather than the visible ones
  n_hidden <- sum(dat <= max_east &
    dat <= max_west &
    dat <= max_north &
    dat <= max_south)

  n_visible <- n_side^2 - n_hidden
  out <- n_visible
  return(out)
}

calc_scenic_score <- function(x, y, dat) {
  
  n_side <- nrow(dat) 

  if (y > 1) {
    north_view <- dat[(y-1):1, x]
    north_score <- min(c(which(north_view >= dat[y,x]), length(north_view)))
  } else {
    north_score <- 0
  }

  if (y < n_side) {
    south_view <- dat[(y+1):n_side, x]
    south_score <- min(c(which(south_view >= dat[y,x]), length(south_view)))
  } else {
    south_score <- 0
  }

  if (x > 1) {
    east_view <- dat[y, (x-1):1]
    east_score <- min(c(which(east_view >= dat[y,x]), length(east_view)))
  } else {
    east_score <- 0
  }

  if (x < n_side) {
    west_view <- dat[y, (x+1):n_side]
    west_score <- min(c(which(west_view >= dat[y,x]), length(west_view)))
  } else {
    west_score <- 0
  }

  out <- north_score * south_score * east_score * west_score
  return(out)  

}

calc_max_scenic_score <- function(path) {
  x <- paste(readLines(path), collapse = "")
  x <- as.numeric(strsplit(x, "")[[1]])
  n_side <- sqrt(length(x))
  dat <- matrix(x, ncol = n_side, byrow = TRUE)
  out <- matrix(NA, ncol = n_side, nrow = n_side)
  for (i in seq_len(n_side)) {
    for (j in seq_len(n_side)) {
      out[i, j] <- calc_scenic_score(i, j, dat)
    }
    print(i)
  }
  return(max(out))
}

# part 1 
count_visible_trees("day08/test_input.txt") == 21
count_visible_trees("day08/input.txt") == 1705

# part 2
calc_max_scenic_score("day08/test_input.txt") == 8
calc_max_scenic_score("day08/input.txt") == 371200

```



# [Day 7: No Space Left On Device](https://adventofcode.com/2022/day/7)

[Reddit Solutions](https://www.reddit.com/r/adventofcode/comments/zesk40/2022_day_7_solutions/)

```R

clean_drive <- function(path, part_one = TRUE) {
  x <- readLines(path)
  # smol modifications to create a real bash script
  x <- gsub("^dir", "mkdir", x)
  x[grep("^\\d", x)] <- paste("fallocate -l", x[grep("^\\d", x)])
  x <- gsub("\\$ ", "", x)
  x <- x[-which(x == "ls")]
  x[which(x == "cd /")] <- "cd _root"
  writeLines(x, "temp.sh")

  if (dir.exists("_root")) unlink("_root", recursive = TRUE)
  dir.create("_root")
  system("sh temp.sh")
  file.remove("temp.sh")

  files <- list.files("_root", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  dat <- file.info(files)
  unlink("_root", recursive = TRUE)
  dirs <- rownames(dat)[which(dat$isdir)]
  dat <- dplyr::filter(dat, !dat$isdir)
  dir_sizes <- rep(NA, length(dirs))
  for (i in seq_along(dirs)) {
    dir_sizes[i] <- sum(dat$size[grep(dirs[i], rownames(dat))])
  } 
  if (part_one == TRUE) {
    out <- sum(dir_sizes[dir_sizes < 100000])
  } else {
    total <- sum(dat$size)
    empty <- 70000000 - total
    to_delete <- 30000000 - empty
    out <- min(dir_sizes[dir_sizes > to_delete])
  }
  return(out)
}


clean_drive("day07/test_input.txt") == 95437
clean_drive("day07/input.txt") == 1449447

clean_drive("day07/test_input.txt", part_one = FALSE) == 24933642
clean_drive("day07/input.txt", part_one = FALSE) == 8679207

```


# [Day 6: Tuning Trouble](https://adventofcode.com/2022/day/6)

[Reddit Solutions](https://www.reddit.com/r/adventofcode/comments/zdw0u6/2022_day_6_solutions/)

```r

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

find_packet_start("day06/test_input_1.txt") == 7
find_packet_start("day06/test_input_2.txt") == 5
find_packet_start("day06/test_input_3.txt") == 6
find_packet_start("day06/test_input_4.txt") == 10
find_packet_start("day06/test_input_5.txt") == 11
find_packet_start("day06/input.txt") == 1361

find_packet_start("day06/test_input_1.txt", chunk_length = 14) == 19
find_packet_start("day06/test_input_2.txt", chunk_length = 14) == 23
find_packet_start("day06/test_input_3.txt", chunk_length = 14) == 23
find_packet_start("day06/test_input_4.txt", chunk_length = 14) == 29
find_packet_start("day06/test_input_5.txt", chunk_length = 14) == 26

find_packet_start("day06/input.txt", chunk_length = 14) == 3263



```


# [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5)


```r

number_scraper <- function(pattern, x) {
  m <- regexpr(pattern, x, perl = TRUE)
  as.numeric(regmatches(x, m))
}

find_top_crates <- function(path, move_type = "one at a time") {

  x <- readLines(path)

  map_bottom <- which(x == "")
  map <- x[1:(map_bottom-2)]
  columns <- nchar(x[1])

  inst <- data.frame(
    text = x[(map_bottom+1):length(x)]
  )

  inst$source <- number_scraper("(?<=from )\\d+", inst$text)
  inst$dest <- number_scraper("(?<=to )\\d+", inst$text)
  inst$n_moves <- number_scraper("(?<=^move )\\d+", inst$text)

  n_cols <- nchar(map[1]) %/% 3

  stacks <- vector("list", n_cols)
  for (i in length(map):1) {
    for (j in 1:n_cols) {
      addy_j <- 4 * (j-1) + 2
      box <- substr(map[i], addy_j, addy_j)
      if (box != " ") {
        stacks[[j]] <- c(stacks[[j]], box)
      }
    }
  }

  for (i in seq_len(nrow(inst))) {
    if (move_type == "one at a time") {
      for (j in seq_len(inst$n_moves[i])) {
        col_len <- length(stacks[[inst$source[i]]])
        stopifnot(col_len > 0)
        stacks[[inst$dest[i]]] <- c(stacks[[inst$dest[i]]], stacks[[inst$source[i]]][col_len])
        stacks[[inst$source[i]]] <- stacks[[inst$source[i]]][-col_len]
      }
    } else if (move_type == "multiple at once") {
      col_len <- length(stacks[[inst$source[i]]])
      stopifnot(col_len > 0)
      targets <- (col_len - inst$n_moves[i] + 1):col_len
      stacks[[inst$dest[i]]] <- c(stacks[[inst$dest[i]]], stacks[[inst$source[i]]][targets])
        stacks[[inst$source[i]]] <- stacks[[inst$source[i]]][-targets]

    } else stop("invalid move_type")
  }

  top_box <- rep(NA, n_cols)
  for (i in seq_len(n_cols)) {
    top_box[i] <- stacks[[i]][length(stacks[[i]])]
  }
  out <- paste(top_box, collapse = "")
  return(out)
}

find_top_crates("day05/test_input.txt") == "CMZ"
find_top_crates("day05/input.txt") == "CWMTGHBDW"

find_top_crates("day05/test_input.txt", move_type = "multiple at once") == "MCD"
find_top_crates("day05/input.txt", move_type = "multiple at once") == "SSCGWJCRB"

```


# [Day 4: Camp Cleanup](https://adventofcode.com/2022/day/4)

```r

number_scraper <- function(pattern, x) {
  m <- regexpr(pattern, x, perl = TRUE)
  as.numeric(regmatches(x, m))
}

count_overlaps <- function(path, type = "subsets") {
  x <- readLines(path)
  A_start <- number_scraper("^\\d+", x)
  A_stop <- number_scraper("\\d+(?=\\,)", x)
  B_start <- number_scraper("(?<=,)\\d+", x)
  B_stop <- number_scraper("\\d+$", x)
  if (type == "subsets") {
    out <- sum(
      A_start <= B_start & A_stop >= B_stop |
      B_start <= A_start & B_stop >= A_stop)
  } else if (type == "all") {
    n_non_overlaps <- sum(
      B_start > A_stop |
      B_stop < A_start
    )
    # so overlaps are everything else
    out <- length(A_start) - n_non_overlaps
  } else {
    stop("invalid type argument")
  }
  return(out)
}

count_overlaps("day04/test_input.txt") == 2
count_overlaps("day04/input.txt") == 542

count_overlaps("day04/test_input.txt", type = "all") == 4
count_overlaps("day04/input.txt", type = "all") == 900

```


# [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3)

```r

rm(list = ls())

inventory_rucksack <- function(pack_string) {
  data.frame(
    part = c(rep(1, nchar(pack_string)/2), rep(2, nchar(pack_string)/2)),
    item = strsplit(pack_string, "")[[1]]
  ) |>
  group_by(part) |>
  count(item) |> as.data.frame() |>
  mutate(priority = match(item, c(letters, LETTERS))) -> inv
  inv$in_both_parts <- inv$item %in% intersect(inv$item[inv$part == 1], inv$item[inv$part == 2])
  return(inv)
}

calc_rucksack_priorities <- function(path, scenario = "misplaced") {
  packs <- data.frame(
    contents = readLines(path),
    priority = NA
  )
  if (scenario == "misplaced") {
    for (i in seq_len(nrow(packs))) {
      inv <- inventory_rucksack(packs$contents[i])
      packs$priority[i] <- inv$priority[which(inv$in_both_parts)][1]
    }
    out <- sum(packs$priority)
  } else if (scenario == "grouping") {
    n_groups <- nrow(packs) %/% 3
    packs$group <- rep(seq_len(n_groups), each = 3)
    for (i in seq_len(n_groups)) {
      for (j in 1:3) {
        add <- inventory_rucksack(packs$contents[which(packs$group == i)[j]])
        add$member <- j
        if (j == 1) {
          inv <- add
        } else {
          inv <- bind_rows(inv, add)
        }
      }
      inv$in_all_members <- inv$item %in% intersect(intersect(inv$item[which(inv$member == 1)], inv$item[which(inv$member == 2)]), inv$item[which(inv$member == 3)])
      packs$priority[which(packs$group == i)] <- inv$priority[which(inv$in_all_members)][1]
    }
    elf_groups <- packs |> group_by(group) |> summarize(priority = first(priority)) 
    out <- sum(elf_groups$priority)
  }
  return(out)
}

calc_rucksack_priorities("day03/test_input.txt") == 157
calc_rucksack_priorities("day03/input.txt") == 7597

calc_rucksack_priorities("day03/test_input.txt", scenario = "grouping") == 70
calc_rucksack_priorities("day03/input.txt", scenario = "grouping") == 2607

```



# [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2)

Each round you play a game. Your score is the shape you selected, 1 = Rock, 2 for paper, 3 for scissors, plus the outcome: 0 for a loss, 3 for a draw, and 6 for a win.

The strategy guide gives each opponent's (pre-determined) choice and the recommended strategy to sneakily cheat. The opponent's choices are coded as:

A = Rock
B = Paper
C = Scissors

In the first version of the problem, the meaning of the response column is which choice you should pick:

X = Rock
Y = Paper
Z = Scissors

Together the opponent's letter and this letter give the score for that round, e.g. A X is a tie.

In the second version of the problem, the meaning of the response column is the desired outcome for that round:

X = lose
Y = draw
Z = win

Together with the first letter, this implies the choice of move that you should make, e.g. A X means you pick scissors.

```r

calc_score <- function(path, buggy = TRUE) {

  x <- readLines(path)

  if (buggy) {

    shape_score <- case_when(
      grepl("X", x) ~ 1,
      grepl("Y", x) ~ 2,
      grepl("Z", x) ~ 3,
    )

    wins <- c("A Y", "B Z", "C X")
    draws <- c("A X", "B Y", "C Z")
    loses <- c("A Z", "B X", "C Y")

    outcome_score <- case_when(
      x %in% wins ~ 6,
      x %in% draws ~ 3,
      x %in% loses ~ 0
    )

  } else {

    outcome_score <- case_when(
      grepl("X", x) ~ 0,
      grepl("Y", x) ~ 3,
      grepl("Z", x) ~ 6,
    )

    rock <- c("B X", "A Y", "C Z")
    paper <- c("C X", "B Y", "A Z")
    scissors <- c("A X", "C Y", "B Z")

    shape_score <- case_when(
      x %in% rock ~ 1,
      x %in% paper ~ 2,
      x %in% scissors ~ 3
    )

  }

  return(sum(shape_score + outcome_score))

}

calc_score("day02/test_input.txt") == 15
calc_score("day02/input.txt") == 8392

calc_score("day02/test_input.txt", buggy = FALSE) == 12
calc_score("day02/input.txt", buggy = FALSE) == 10116

```




# [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1)

```r

rm(list = ls())

library(dplyr)

count_calories <- function(path, n_elves = 1) {
  x <- readLines(path)
  x <- as.numeric(x)
  breaks <- which(is.na(x))
  starts <- c(1, breaks+1)
  stops <- c(breaks-1, length(x))

  for (i in seq_along(starts)) {
    add <- data.frame(
      elf = i,
      calories = x[starts[i]:stops[i]]
    )
    if (i == 1) {
      dat <- add
    } else {
      dat <- bind_rows(dat, add)
    }
  }

  dat |> group_by(elf) |> summarize(total_calories = sum(calories)) |> as.data.frame() |> arrange(desc(total_calories)) -> elves

  return(sum(elves$total_calories[1:n_elves]))
}

count_calories("day01/test_input.txt") == 24000
count_calories("day01/input.txt") == 67633

count_calories("day01/test_input.txt", n_elves = 3) == 45000
count_calories("day01/input.txt", n_elves = 3) == 199628

```