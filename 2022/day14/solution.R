
library(tictoc)
library(dplyr)

viz_map <- function(map, grain = NULL, cell_size = 0.5) {

  sand_col <- "#f9c83d"
  grain_col <- "#fbd955"
  rock_col <- "#988317"

  has_grain <- !is.null(grain)

  rock_points <- which(map == "#", arr.ind = TRUE)
  rocks <- data.frame(
    x = rock_points[,2],
    y = rock_points[,1],
    type = "rock",
    col = rock_col
  )

  has_sand <- any(map == "o")
  if (has_sand) {
    sand_points <- which(map == "o", arr.ind = TRUE)
    sand <- data.frame(
      x = sand_points[,2],
      y = sand_points[,1],
      type = "sand",
      col = sand_col
    )
  }

  par(bg = "black")
  plot(NULL, xlim = c(500 - 50, 500 + 50), ylim = c(1, nrow(map)))
  points(rocks, col = rock_col, pch = 15, cex = cell_size)
  if (has_sand) points(sand, col = sand_col, pch = 15, cex = cell_size)
  if (has_grain) points(grain, col = grain_col, pch = 15, cex = cell_size)

}




sim_grains <- function(path, viz = FALSE, overflow = FALSE) {

  x <- readLines(path)

  x <- x[-which(duplicated(x))]

  # add rocks to the map
  rocks <- data.frame(x = integer(), y = integer())
  for (i in seq_along(x)) {
    coords <- strsplit(x[i], split = " -> ")[[1]]
    for (j in 1:(length(coords) - 1)) {
      x_start <- as.integer(substr(coords[j], 1, regexpr(",", coords[j]) - 1)) + 1L
      y_start <- as.integer(substr(coords[j], regexpr(",", coords[j]) + 1, nchar(coords[j]))) + 1L
      x_stop <- as.integer(substr(coords[j + 1], 1, regexpr(",", coords[j + 1]) - 1)) + 1L
      y_stop <- as.integer(substr(coords[j + 1], regexpr(",", coords[j + 1]) + 1, nchar(coords[j + 1]))) + 1L
      if (y_start != y_stop & x_start == x_stop) {
        add <- data.frame(x = x_start, y = y_start:y_stop)
        rocks <- bind_rows(rocks, add)
      } else if (y_start == y_stop & x_start != x_stop) {
        add <- data.frame(x = x_start:x_stop, y = y_start)
        rocks <- bind_rows(rocks, add)
      } else {
        stop("invalid coordinates")
      }
    }
  }

  map_nrow <- max(rocks$y) + 2L

  map <- matrix(".", nrow = map_nrow, ncol = 1300) # give a healthy width

  # add a bottom line of bedrock
  map[1,] <- "#"

  # need to substract from nrow(map), so the map sees the top row as nrow(map) rather than 1
  rocks$y <- map_nrow - rocks$y + 1L

  for (i in 1:nrow(rocks)) map[rocks$y[i], rocks$x[i]] <- "#"

  stopifnot(all(rocks$y > 0))
  stopifnot(all(rocks$x > 0))

  if (viz) viz_map(map)

  stop_condition <- FALSE

  while (!stop_condition) {
    grain <- data.frame(x = 501L, y = nrow(map))
    if (map[nrow(map), 501L] == "o") stop_condition <- TRUE
    while (!stop_condition & map[grain$y, grain$x] == ".") {
      if (!overflow & grain$y == 2L) {
        # only trigger if overflow set to FALSE
        stop_condition <- TRUE
      } else if (map[grain$y-1, grain$x] == ".") {
        grain$y <- grain$y - 1
      } else if (map[grain$y-1, grain$x-1] == ".") {
        grain$y <- grain$y - 1
        grain$x <- grain$x - 1
      } else if (map[grain$y-1, grain$x+1] == ".") {
        grain$y <- grain$y - 1
        grain$x <- grain$x + 1
      } else {
        map[grain$y, grain$x] <- "o"
      }
      if (viz) {
        viz_map(map, grain)
        Sys.sleep(0.01)
      }
    }
  }
  if (viz) viz_map(map, grain)
  out <- sum(map == "o")
  return(out)
}


# sim_grains("day14/test_input.txt") == 24

tic("Day 14, part 1")
stopifnot(sim_grains("day14/input.txt") == 913)
toc(log=TRUE)

# sim_grains("day14/test_input.txt", overflow = TRUE) == 93

tic("Day 14, part 2")
stopifnot(sim_grains("day14/input.txt", overflow = TRUE) == 30762)
toc(log=TRUE)
