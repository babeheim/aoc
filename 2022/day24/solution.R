
library(tictoc)
library(dplyr)

con_yx_i <- function(y, x, n) {
  (x - 1) * n + y
}
con_i_xy <- function(i, n) {
  data.frame(
    y = (i - 1) %% n + 1,
    x = (i - 1) %/% n + 1
  )
}

update_blizzards <- function(blizzards, map_rows, map_cols) {

  if (">" %in% blizzards$direction) {
    east <- which(blizzards$direction == ">")
    blizzards$col[east] <- blizzards$col[east] + 1L
    # wraparound check
    if (any(blizzards$col == map_cols)) {
      tar <- which(blizzards$col == map_cols)
      blizzards$col[tar] <- 2L
    }
  }
  
  if ("<" %in% blizzards$direction) {
    west <- which(blizzards$direction == "<")
    blizzards$col[west] <- blizzards$col[west] - 1L
    # wraparound check
    if (any(blizzards$col == 1L)) {
      tar <- which(blizzards$col == 1L)
      blizzards$col[tar] <- (map_cols - 1L)
    }
  }

  if ("^" %in% blizzards$direction) {
    north <- which(blizzards$direction == "^")
    blizzards$row[north] <- blizzards$row[north] - 1L
    # wraparound check
    if (any(blizzards$row == 1L)) {
      tar <- which(blizzards$row == 1L)
      blizzards$row[tar] <- (map_rows - 1L)
    }
  }

  if ("v" %in% blizzards$direction) {
    south <- which(blizzards$direction == "v")
    blizzards$row[south] <- blizzards$row[south] + 1L
    # wraparound check
    if (any(blizzards$row == map_rows)) {
      tar <- which(blizzards$row == map_rows)
      blizzards$row[tar] <- 2L
    }
  }
  
  blizzards$i <- con_yx_i(blizzards$row, blizzards$col, map_rows)

  return(blizzards)

}


identify_open_spots <- function(blizzards, walls, map_rows, map_cols) {

  # (entrance is always the 1,2 cell, exit the n, (m-1) cell)
  open <- data.frame(
    row = c(1, map_rows),
    col = c(2, map_cols-1)
  )

  open$i <- con_yx_i(open$row, open$col, map_rows)

  open_i <- which(!(1:(map_rows*map_cols) %in% c(walls$i, blizzards$i)))

  if (length(open_i) > 0) {
    add <- data.frame(
      i = open_i,
      row = con_i_xy(open_i, map_rows)[,"y"],
      col = con_i_xy(open_i, map_rows)[,"x"]
    )

    open <- bind_rows(open, add)
  }

  return(open)

}



find_possible_paths <- function(sys, open, map_rows) {
  tar <- which(sys[,1])
  for (i in tar) {
    here_open <- i %in% open$i
    if (here_open) sys[i,2] <- TRUE
    north_open <- (i-1) %in% open$i
    if (north_open) sys[(i-1),2] <- TRUE
    south_open <- (i+1) %in% open$i
    if (south_open) sys[(i+1),2] <- TRUE
    east_open <- (i + map_rows) %in% open$i
    if (east_open) sys[(i + map_rows),2] <- TRUE
    west_open <- (i - map_rows) %in% open$i
    if (west_open) sys[(i - map_rows),2] <- TRUE
  }

  return(sys)

}


sim_futures <- function(path, more_legs = FALSE) {

  x <- readLines(path)
  map <- matrix(NA, ncol = nchar(x[1]), nrow = length(x))
  for (i in 1:length(x)) {
    map[i,] <- strsplit(x[i], split = "")[[1]]
  }

  map_cols <- ncol(map)
  map_rows <- nrow(map)

  walls <- as.data.frame(which(map == "#", arr.ind = TRUE))
  walls$i <- con_yx_i(walls$row, walls$col, map_rows)

  blizzards <- data.frame(row = integer(), col = integer(), direction = character())

  if (any(grepl(">", map))) {
    add <- as.data.frame(which(map == ">", arr.ind = TRUE))
    add$direction <- ">"
    blizzards <- bind_rows(blizzards, add)
  }

  if (any(grepl("<", map))) {
    add <- as.data.frame(which(map == "<", arr.ind = TRUE))
    add$direction <- "<"
    blizzards <- bind_rows(blizzards, add)
  }

  if (any(grepl("v", map))) {
    add <- as.data.frame(which(map == "v", arr.ind = TRUE))
    add$direction <- "v"
    blizzards <- bind_rows(blizzards, add)
  }

  if (any(grepl("^", map))) {
    add <- as.data.frame(which(map == "^", arr.ind = TRUE))
    add$direction <- "^"
    blizzards <- bind_rows(blizzards, add)
  }

  blizzards$i <- con_yx_i(blizzards$row, blizzards$col, map_rows)

  open <- data.frame(
    row = c(1, map_rows),
    col = c(2, map_cols-1)
  )

  open$i <- con_yx_i(open$row, open$col, map_rows)

  open_i <- which(!(1:(map_rows*map_cols) %in% c(walls$i, blizzards$i)))

  if (length(open_i) > 0) {
    add <- data.frame(
      i = open_i,
      row = con_i_xy(open_i, map_rows)[,"y"],
      col = con_i_xy(open_i, map_rows)[,"x"]
    )

    open <- bind_rows(open, add)
  }

  # reachability is always a 2-column matrix, for now and next time step
  sys <- matrix(FALSE, nrow = map_rows*map_cols, ncol = 2)

  time <- 0L

  # in the first leg, we start at the top-left entrance to the maze
  sys[open$i[1],1] <- TRUE

  goal_i <- open$i[2]

  # check whether this leg's goal-state has been reached
  leg_complete <- sys[goal_i,1]

  while (!leg_complete) {
    # predict blizzard locations next time step
    blizzards <- update_blizzards(blizzards, map_rows, map_cols)    
    # predict open spots in the next time step
    open <- identify_open_spots(blizzards, walls, map_rows, map_cols)
    # having predicted all open spots, now identify the ones that are reachable in the next time step
    sys <- find_possible_paths(sys, open, map_rows)

    # now iterate the loop: the predicted future becomes the present,
    # the time iterates by 1
    sys[,1] <- sys[,2]
    sys[,2] <- FALSE
    time <- time + 1L
    # if (time %% 50 == 0) print(time)
    # and we check if the goal has been reached
    leg_complete <- sys[goal_i,1]
  }

  if (!more_legs) return(time)

  # prune all paths except one
  sys[,1] <- FALSE
  sys[goal_i,1] <- TRUE

  # in leg 2, we have to start back from the goal to the entrance
  goal_i <- open$i[1]
  
  # check whether the leg's goal-state has been reached
  leg_complete <- sys[goal_i,1]

  while (!leg_complete) {
    # predict blizzard locations next time step
    blizzards <- update_blizzards(blizzards, map_rows, map_cols)    
    # predict open spots in the next time step
    open <- identify_open_spots(blizzards, walls, map_rows, map_cols)
    # having predicted all open spots, now identify the ones that are reachable in the next time step
    sys <- find_possible_paths(sys, open, map_rows)

    # now iterate the loop: the predicted future becomes the present,
    # the time iterates by 1
    sys[,1] <- sys[,2]
    sys[,2] <- FALSE
    time <- time + 1L
    # if (time %% 50 == 0) print(time)
    # and we check if the goal has been reached
    leg_complete <- sys[goal_i,1]
  }

  # prune all paths except one
  sys[,1] <- FALSE
  sys[goal_i,1] <- TRUE

  # in leg 3, we have to start at the entrance again and get back to the goal
  goal_i <- open$i[2]
  
  # check whether the leg's goal-state has been reached
  leg_complete <- sys[goal_i,1]

  while (!leg_complete) {
    # predict blizzard locations next time step
    blizzards <- update_blizzards(blizzards, map_rows, map_cols)    
    # predict open spots in the next time step
    open <- identify_open_spots(blizzards, walls, map_rows, map_cols)
    # having predicted all open spots, now identify the ones that are reachable in the next time step
    sys <- find_possible_paths(sys, open, map_rows)

    # now iterate the loop: the predicted future becomes the present,
    # the time iterates by 1
    sys[,1] <- sys[,2]
    sys[,2] <- FALSE
    time <- time + 1L
    # if (time %% 50 == 0) print(time)
    # and we check if the goal has been reached
    leg_complete <- sys[goal_i,1]
  }

  return(time)

}

stopifnot(sim_futures("day24/test_input.txt") == 18)

tic("Day 24, part 1")
stopifnot(sim_futures("day24/input.txt") == 247)
toc(log=TRUE)

stopifnot(sim_futures("day24/test_input.txt", more_legs = TRUE) == 54)

tic("Day 24, part 2")
stopifnot(sim_futures("day24/input.txt", more_legs = TRUE) == 728)
toc(log=TRUE)
