
library(tictoc)

calc_movement <- function(col, row, facing, brd, n_steps) {

  curr_row <- row
  curr_col <- col
  report <- paste("attempting", n_steps, "steps to the", c("East", "South", "West", "North")[facing + 1])
  for (step in seq_len(n_steps)) {
    if (facing == 0L) {
      # facing east
      cand_col <- curr_col + 1L
      cand_row <- curr_row
      cand_cell <- brd[cand_row, cand_col]
      # wraparound check
      if (is.na(cand_cell)) {
        if (all(is.na(brd[cand_row,]))) stop("off the map!")
        cand_col <- min(which(!is.na(brd[cand_row,])))
        cand_cell <- brd[cand_row, cand_col]
        if (cand_cell) report <- paste0(report, "; teleported")
      }
    } else if (facing == 1L) {
      # facing south
      cand_col <- curr_col
      cand_row <- curr_row + 1L
      cand_cell <- brd[cand_row, cand_col]
      # wraparound check
      if (is.na(cand_cell)) {
        if (all(is.na(brd[,cand_col]))) stop("off the map!")
        cand_row <- min(which(!is.na(brd[,cand_col])))
        cand_cell <- brd[cand_row, cand_col]
        if (cand_cell) report <- paste0(report, "; teleported")
      }
    } else if (facing == 2L) {
      # facing west
      cand_col <- curr_col - 1L
      cand_row <- curr_row
      cand_cell <- brd[cand_row, cand_col]
      # wraparound check
      if (is.na(cand_cell)) {
        if (all(is.na(brd[cand_row,]))) stop("off the map!")
        cand_col <- max(which(!is.na(brd[cand_row,])))
        cand_cell <- brd[cand_row, cand_col]
        if (cand_cell) report <- paste0(report, "; teleported")
      }
    } else if (facing == 3L) {
      # facing north
      cand_col <- curr_col
      cand_row <- curr_row - 1L
      cand_cell <- brd[cand_row, cand_col]
      # wraparound check
      if (is.na(cand_cell)) {
        if (all(is.na(brd[,cand_col]))) stop("off the map!")
        cand_row <- max(which(!is.na(brd[,cand_col])))
        cand_cell <- brd[cand_row, cand_col]
        if (cand_cell) report <- paste0(report, "; teleported")
      }
    } else {
      stop("facing invalid")
    }
    # now that we've identified the next tile, is it passable?
    stopifnot(!is.na(cand_cell))
    if (cand_cell) {
      curr_row <- cand_row
      curr_col <- cand_col
    } else {
      report <- paste0(report, paste("; hit barrier after", step - 1, "steps"))
      break()
    }
  }
  out <- data.frame(row = curr_row, col = curr_col, report = report)
  return(out)
}



sim_movement <- function(path, viz = FALSE) {

  x <- readLines(path)

  # load board
  board_lines <- x[1:(which(x == "") - 1)]
  brd <- matrix(NA, nrow = length(board_lines) + 2L, ncol = max(nchar(board_lines) + 2L))
  for (i in 1:length(board_lines)) {
    chars <- strsplit(board_lines[i], split = "")[[1]]
    for (j in 1:length(chars)) {
      if (chars[j] == ".") {
        brd[i + 1L, j + 1L] <- TRUE
      } else if (chars[j] == "#") {
        brd[i + 1L, j + 1L] <- FALSE
      }
    }
  }

  # since the board is really a cube, we need another matrix that decides how each face connects together...
  # six faces each has four boundaries so 24 boundaries
  # each entry boundary has a destination boundary and instructions on how the facing changes and the rows change
  # it would probably be easier to simply keep the six faces separate from each other and make simplier rules....

  # further the layouts are different in the test and final input
  # i want a single system!

  # generally 3 x 4 so whats the divisor?
  
  n_side <- min(dim(brd) - 2) / 3 
  (dim(brd) - 2) %/% n_side # 3 x 4




  # move logic: given a board position (row and column), a heading and a number of moves, figure out where the resulting new position is

  # Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).

  mv_string <- x[length(x)]
  mv_string <- gsub("\\s", "", mv_string)
  mv_string <- gsub("R", ",R,", mv_string)
  mv_string <- gsub("L", ",L,", mv_string)
  mv_string <- gsub(",+", ",", mv_string)
  instructions <- strsplit(mv_string, split = ",")[[1]]
  n_instructions <- length(instructions)

  # You begin the path in the leftmost open tile of the top row of tiles. Initially, you are facing to the right (from the perspective of how the map is drawn).
  states <- data.frame(
    row = 2,
    col = min(which(brd[2,])),
    facing = 0
  )

  states$last_action <- NA
  states$last_msg <- ""

  # apply each movement and update the state
  for (i in seq_len(n_instructions)) {
    last <- nrow(states)
    new <- data.frame(
      last_action = instructions[i],
      row = NA,
      col = NA,
      facing = NA
    )
    if (instructions[i] == "R") {
      new$row <- states$row[last]
      new$col <- states$col[last]
      new$facing <- (states$facing[last] + 1L) %% 4
      new$last_msg <- paste("turned right; now facing", c("East", "South", "West", "North")[new$facing + 1])
    } else if (instructions[i] == "L") {
      new$row <- states$row[last]
      new$col <- states$col[last]
      new$facing <- (states$facing[last] - 1L) %% 4
      new$last_msg <- paste("turned left; now facing", c("East", "South", "West", "North")[new$facing + 1])
    } else {
      new$facing <- states$facing[last]
      n_steps <- as.integer(instructions[i])
      calcs <- calc_movement(states$col[last], states$row[last], states$facing[last], brd, n_steps)
      new$last_msg <- calcs$report
      new$row <- calcs$row
      new$col <- calcs$col
    }
    states <- bind_rows(states, new)
  }

  if (viz == TRUE) {
    par(bg = "black")
    plot(NULL, xlim = c(1, ncol(brd)), ylim = c(nrow(brd), 1))
    for (i in 1:nrow(brd)) {
      for (j in 1:ncol(brd)) {
        if (!is.na(brd[i,j])) {
          if (brd[i,j]) {
            cell_col <- "#00d9ff"
          } else {
            cell_col <- "#0051ff"
          }
          rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, col = cell_col, border = NA)
        }
      }
    }
  }

  out <- (states[nrow(states),]$row - 1L) * 1000 +
  (states[nrow(states),]$col - 1L) * 4 +
  states[nrow(states),]$facing

  return(out)

}


stopifnot(sim_movement("day22/test_input.txt") == 6032)

tic("Day 22, part 1")
stopifnot(sim_movement("day22/input.txt") == 197160)
toc(log=TRUE)


# new approach for part 2

rotate_matrix_cw <- function(mat, n_times = 0) {
  n_times <- n_times %% 4
  if (n_times == 0) {
    out <- mat
  } else if (n_times == 1) {
    out <- t(mat)[,ncol(mat):1]
  } else if (n_times == 2) {
    # aka -2
    out <- t(mat)[,ncol(mat):1]
    out <- t(out)[,ncol(out):1]
  } else if (n_times == 3) {
    # aka -1
    out <- t(mat)[nrow(mat):1,]
  } else {
    stop("n_times invalid")
  }
  return(out)
}

rotate_position <- function(pos, n_side, rotation) {
  map <- matrix(1:(n_side^2), ncol = n_side)
  position_index <- map[pos$row, pos$col]
  map <- rotate_matrix_cw(map, n_times = rotation)
  new_position <- which(map == position_index, arr.ind = TRUE)
  out <- pos
  out$row <- as.integer(new_position[,"row"])
  out$col <- as.integer(new_position[,"col"])
  out$orientation <- as.integer((pos$orientation + rotation) %% 4)
  return(out)
}

move_once <- function(current_state, edges, terrain) {

  n_side <- dim(terrain)[1]

  if (current_state$orientation == 0L) {
    move <- list(row = 0L, col = 1L)
  } else if (current_state$orientation == 1L) {
    move <- list(row = 1L, col = 0L)
  } else if (current_state$orientation == 2L) {
    move <- list(row = 0L, col = -1L)
  } else if (current_state$orientation == 3L) {
    move <- list(row = -1L, col = 0L)
  } else {
    stop("orientation invalid")
  }

  next_state <- current_state
  next_state$row <- current_state$row + move$row
  next_state$col <- current_state$col + move$col

  if (next_state$col == 0L) {
    tar <- which(next_state$face == edges$from & edges$dir == "W")
    next_state$face <- edges$to[tar]
    next_state$col <- n_side
    next_state <- rotate_position(next_state, n_side, edges$rotation[tar])
  } else if (next_state$col == (n_side + 1L)) {
    tar <- which(next_state$face == edges$from & edges$dir == "E")
    next_state$face <- edges$to[tar]
    next_state$col <- 1L
    next_state <- rotate_position(next_state, n_side, edges$rotation[tar])
  } else if (next_state$row == 0L) {
    tar <- which(next_state$face == edges$from & edges$dir == "N")
    next_state$face <- edges$to[tar]
    next_state$row <- n_side
    next_state <- rotate_position(next_state, n_side, edges$rotation[tar])
  } else if (next_state$row == (n_side + 1L)) {
    tar <- which(next_state$face == edges$from & edges$dir == "S")
    next_state$face <- edges$to[tar]
    next_state$row <- 1L
    next_state <- rotate_position(next_state, n_side, edges$rotation[tar])
  }

  # having identified the proposed state, check whether it is blocked on the terrain map

  if (terrain[next_state$row, next_state$col, next_state$face] == 0L) {
    return(next_state)
  } else {
    return(current_state)
  }

}

sim_movement2 <- function(path, verbose = FALSE) {

  x <- readLines(path)

  # describing a cube net for part 2
  edges <- list(
    list(from = 1, to = 2, dir = "N", rotation = 2),
    list(from = 1, to = 4, dir = "S", rotation = 0),
    list(from = 1, to = 6, dir = "E", rotation = 1),
    list(from = 1, to = 3, dir = "W", rotation = -1),

    list(from = 2, to = 1, dir = "N", rotation = 2),
    list(from = 2, to = 5, dir = "S", rotation = 2),
    list(from = 2, to = 3, dir = "E", rotation = 0),
    list(from = 2, to = 6, dir = "W", rotation = 0),

    list(from = 3, to = 1, dir = "N", rotation = 1),
    list(from = 3, to = 5, dir = "S", rotation = -1),
    list(from = 3, to = 4, dir = "E", rotation = 0),
    list(from = 3, to = 2, dir = "W", rotation = 0),

    list(from = 4, to = 1, dir = "N", rotation = 0),
    list(from = 4, to = 5, dir = "S", rotation = 0),
    list(from = 4, to = 6, dir = "E", rotation = 0),
    list(from = 4, to = 3, dir = "W", rotation = 0),

    list(from = 5, to = 4, dir = "N", rotation = 0),
    list(from = 5, to = 2, dir = "S", rotation = 2),
    list(from = 5, to = 6, dir = "E", rotation = -1),
    list(from = 5, to = 3, dir = "W", rotation = 1),
    
    list(from = 6, to = 1, dir = "N", rotation = -1),
    list(from = 6, to = 5, dir = "S", rotation = 1),
    list(from = 6, to = 2, dir = "E", rotation = 0),
    list(from = 6, to = 4, dir = "W", rotation = 0)
  ) |> bind_rows() |> as.data.frame()

  if (path == "day22/test_input.txt") {
    brd_map <- list(
      list(face = 1, rows_before = 0, cols_before = 2, rotation = 0),
      list(face = 2, rows_before = 1, cols_before = 0, rotation = 0),
      list(face = 3, rows_before = 1, cols_before = 1, rotation = 0),
      list(face = 4, rows_before = 1, cols_before = 2, rotation = 0),
      list(face = 5, rows_before = 2, cols_before = 2, rotation = 0),
      list(face = 6, rows_before = 2, cols_before = 3, rotation = -1)
    ) |> bind_rows() |> as.data.frame()
  } else if (path == "day22/input.txt") {
    brd_map <- list(
      list(face = 1, rows_before = 0, cols_before = 1, rotation = 0),
      list(face = 2, rows_before = 3, cols_before = 0, rotation = 1),
      list(face = 3, rows_before = 2, cols_before = 0, rotation = 1),
      list(face = 4, rows_before = 1, cols_before = 1, rotation = 0),
      list(face = 5, rows_before = 2, cols_before = 1, rotation = 0),
      list(face = 6, rows_before = 0, cols_before = 2, rotation = 1)
    ) |> bind_rows() |> as.data.frame()
  } else {
    stop("invalid path")
  }

  edges$rotation <- as.integer(edges$rotation)
  edges$from <- as.integer(edges$from)
  edges$to <- as.integer(edges$to)

  # load board
  board_lines <- x[1:(which(x == "") - 1)]
  brd <- matrix(NA, nrow = length(board_lines), ncol = max(nchar(board_lines)))
  for (i in 1:length(board_lines)) {
    chars <- strsplit(board_lines[i], split = "")[[1]]
    for (j in 1:length(chars)) {
      if (chars[j] == ".") {
        brd[i, j] <- 0L
      } else if (chars[j] == "#") {
        brd[i, j] <- 1L
      }
    }
  }

  n_side <- min(dim(brd)) / 3 

  viz <- FALSE
  if (viz == TRUE) {
    par(bg = "black")
    plot(NULL, xlim = c(1, ncol(brd)), ylim = c(nrow(brd), 1))
    for (i in 1:nrow(brd)) {
      for (j in 1:ncol(brd)) {
        if (!is.na(brd[i,j])) {
          if (brd[i,j]) {
            cell_col <- "#00d9ff"
          } else {
            cell_col <- "#0051ff"
          }
          rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, col = cell_col, border = NA)
        }
      }
    }
    for (i in 1:nrow(brd_map)) {
      rect(brd_map$cols_before[i] * n_side + 1, brd_map$rows_before[i] * n_side + 1, brd_map$cols_before[i] * n_side + n_side, brd_map$rows_before[i] * n_side + n_side, col = "red")
      text(
        brd_map$cols_before[i] * n_side + (n_side + 1)/2,
        brd_map$rows_before[i] * n_side + (n_side + 1)/2,
        labels = i,
        srt = 90 * brd_map$rotation[i]
      )
    }
  }

  # map the real terrain from the brd object
 
  terrain <- array(NA, dim = c(n_side, n_side, 6))

  for (i in 1:6) {
    rs <- brd_map$rows_before[i] * n_side + 1
    cs <- brd_map$cols_before[i] * n_side + 1
    terrain[,,i] <- rotate_matrix_cw(brd[rs:(rs+(n_side-1)),cs:(cs+(n_side-1))], brd_map$rotation[i])
  }

  mv_string <- x[length(x)]
  mv_string <- gsub("\\s", "", mv_string)
  mv_string <- gsub("R", ",R,", mv_string)
  mv_string <- gsub("L", ",L,", mv_string)
  mv_string <- gsub(",+", ",", mv_string)
  instructions <- strsplit(mv_string, split = ",")[[1]]
  n_instructions <- length(instructions)

  # initial state: You begin the path in the leftmost open tile of the top row of tiles. Initially, you are facing to the right (from the perspective of how the map is drawn).
  state <- list(row = 1L, col = 1L, face = 1L, orientation = 0L)

  for (i in seq_len(n_instructions)) {
    if (instructions[i] == "L") {
      state$orientation <- as.integer((state$orientation - 1L) %% 4)
      if (verbose) cat("turned left\n")
    } else if (instructions[i] == "R") {
      state$orientation <- as.integer((state$orientation + 1L) %% 4)
      if (verbose) cat("turned right\n")
    } else {
      n_moves <- as.integer(instructions[i])
      for (j in seq_len(n_moves)) {
        last_row <- state$row
        last_col <- state$col
        last_face <- state$face
        state <- move_once(state, edges, terrain)
        if (state$row == last_row & state$col == last_col & state$face == last_face) {
          if (verbose) cat("moved", (j-1), "steps; hit barrier\n")
          break()
        }
        if (j == n_moves) {
          if (verbose) cat("moved", j, "steps\n")
        }
      }
    }
  }

  # translate everything back to the board coordinate system, including orientation!
  brd_state <- rotate_position(state, n_side, (-1) * brd_map$rotation[state$face])
  brd_row <- brd_map$rows_before[brd_state$face] * n_side + brd_state$row
  brd_col <- brd_map$cols_before[brd_state$face] * n_side + brd_state$col

  out <- brd_row * 1000 +
    brd_col * 4 +
    brd_state$orientation

  return(out)

}

stopifnot(sim_movement2("day22/test_input.txt") == 5031)

tic("day 22, part 2")
stopifnot(sim_movement2("day22/input.txt") == 145065)
toc(log = TRUE)

