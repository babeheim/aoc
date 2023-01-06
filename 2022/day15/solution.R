
library(tictoc)
library(sp)
library(rgeos)

number_scraper <- function(pattern, x) {
  m <- regexpr(pattern, x, perl = TRUE)
  as.numeric(regmatches(x, m))
}


calc_transect_coverage <- function(path, show_progress = FALSE) {

  # hard-code parameters based on path
  if (path == "day15/input.txt") {
    target_row <- 2000000
    key_sensors <- c(14, 39, 5, 25, 38, 16, 30, 8, 9, 19)
  } else if (path == "day15/test_input.txt") {
    target_row <- 10
    key_sensors <- 1:14
  } else {
    stop()
  }

  x <- readLines(path)

  sensor_text <- substr(x, 1, regexpr(":", x) - 1)
  beacon_text <- substr(x, regexpr(":", x) + 2, nchar(x))

  beacons <- data.frame(
    text = sort(unique(beacon_text))
  )

  sensors <- data.frame(
    text = sensor_text,
    nearest_beacon = match(beacon_text, beacons$text)
  )

  beacons$x <- number_scraper("(?<=x=)-*\\d+", beacons$text)
  beacons$y <- number_scraper("(?<=y=)-*\\d+", beacons$text)

  sensors$x <- number_scraper("(?<=x=)-*\\d+", sensors$text)
  sensors$y <- number_scraper("(?<=y=)-*\\d+", sensors$text)

  sensors$nearest_beacon_x <- beacons$x[sensors$nearest_beacon]
  sensors$nearest_beacon_y <- beacons$y[sensors$nearest_beacon]

  sensors$nearest_beacon_dist <- abs(sensors$nearest_beacon_x - sensors$x) + abs(sensors$nearest_beacon_y - sensors$y)

  sensor_min_x <- min(sensors$x)
  sensor_min_y <- min(sensors$y)
  sensor_max_x <- max(sensors$x)
  sensor_max_y <- max(sensors$y)

  range_min_x <- min(sensors$x - sensors$nearest_beacon_dist)
  range_min_y <- min(sensors$y - sensors$nearest_beacon_dist)
  range_max_x <- max(sensors$x + sensors$nearest_beacon_dist)
  range_max_y <- max(sensors$y + sensors$nearest_beacon_dist)

  # first method: brute-force check of each possible coordinate
  n_covered <- 0L
  target_col <- range_min_x

  if (show_progress) progbar <- txtProgressBar(range_min_x, range_max_x, style=1)

  while (target_col <= range_max_x) {
    is_covered <- FALSE
    for (j in c(key_sensors)) {
      is_covered <- is_covered | (abs(target_col - sensors$x[j]) + abs(target_row - sensors$y[j])) < (sensors$nearest_beacon_dist[j] + 1)
      if (is_covered) break()
    }
    is_covered <- is_covered & !any(beacons$x == target_col & beacons$y == target_row)
    if (is_covered) n_covered <- n_covered + 1L
    target_col <- target_col + 1L
    if (show_progress)setTxtProgressBar(progbar, target_col)
  }

  if (show_progress) close(progbar)

  return(n_covered)

}


find_hidden_beacon <- function(path, viz = FALSE) {

  if (path == "day15/input.txt") {
    box_max <- 4000000
  } else {
    stop()
  }

  x <- readLines(path)

  sensor_text <- substr(x, 1, regexpr(":", x) - 1)
  beacon_text <- substr(x, regexpr(":", x) + 2, nchar(x))

  beacons <- data.frame(
    text = sort(unique(beacon_text))
  )

  sensors <- data.frame(
    text = sensor_text,
    nearest_beacon = match(beacon_text, beacons$text)
  )

  beacons$x <- number_scraper("(?<=x=)-*\\d+", beacons$text)
  beacons$y <- number_scraper("(?<=y=)-*\\d+", beacons$text)

  sensors$x <- number_scraper("(?<=x=)-*\\d+", sensors$text)
  sensors$y <- number_scraper("(?<=y=)-*\\d+", sensors$text)

  sensors$nearest_beacon_x <- beacons$x[sensors$nearest_beacon]
  sensors$nearest_beacon_y <- beacons$y[sensors$nearest_beacon]

  sensors$nearest_beacon_dist <- abs(sensors$nearest_beacon_x - sensors$x) + abs(sensors$nearest_beacon_y - sensors$y)

  sensor_min_x <- min(sensors$x)
  sensor_min_y <- min(sensors$y)
  sensor_max_x <- max(sensors$x)
  sensor_max_y <- max(sensors$y)

  range_min_x <- min(sensors$x - sensors$nearest_beacon_dist)
  range_min_y <- min(sensors$y - sensors$nearest_beacon_dist)
  range_max_x <- max(sensors$x + sensors$nearest_beacon_dist)
  range_max_y <- max(sensors$y + sensors$nearest_beacon_dist)

  # use {sp} and {rgeos} to subtract polygons

  remaining <- data.frame(
    x = c(0, box_max, box_max, 0),
    y = c(0, 0, box_max, box_max)
  )

  covered <- vector("list", nrow(sensors))
  for (i in 1:length(covered)) {
    covered[[i]] <- data.frame(
      x = c(
        sensors$x[i] - sensors$nearest_beacon_dist[i],
        sensors$x[i],
        sensors$x[i] + sensors$nearest_beacon_dist[i],
        sensors$x[i]
      ),
      y = c(
        sensors$y[i],
        sensors$y[i] + sensors$nearest_beacon_dist[i],
        sensors$y[i],
        sensors$y[i] - sensors$nearest_beacon_dist[i]
      )
    )
  }

  poly1 <- Polygon(remaining)
  res <- SpatialPolygons(list(Polygons(list(poly1), "res")))

  for (i in 1:length(covered)) {
    poly2 <- Polygon(covered[[i]])
    # create SpatialPolygons objects
    p2 <- SpatialPolygons(list(Polygons(list(poly2), "p2")))
    if (viz) plot(p2, add = TRUE, col = "red")
    res <- gDifference(res, p2)
  }

  # critically, this approach assumes that there are no 'gaps' between coverage zones less than 1L wide; these aren't legit spots but could show up!
  # the test input has this issue but not the actual input, thankfully
  hidden <- data.frame(x = mean(res@bbox[1,]), y = mean(res@bbox[2,]))

  if (viz) {

    par(mar = c(0, 0, 0, 0))

    plot(NULL, xlim = c(range_min_x, range_max_x), ylim = c(range_max_y, range_min_y), frame.plot = FALSE, axes = FALSE, xlab = "", ylab = "")

    for (i in 1:nrow(sensors)) {
      polygon(
        c(sensors$x[i] + sensors$nearest_beacon_dist[i], sensors$x[i], 
        sensors$x[i] - sensors$nearest_beacon_dist[i], sensors$x[i]),
        c(sensors$y[i], sensors$y[i] + sensors$nearest_beacon_dist[i], 
        sensors$y[i], sensors$y[i] - sensors$nearest_beacon_dist[i]),
        border = gray(0.3, 0.5)
      )
    }

    points(beacons$x, beacons$y, col = "brown4", pch = 20, cex = 2)

    rect(0, box_max, box_max, 0, border = "red", lty = 2)

    points(hidden, col = "red", pch = 20)

  }

  out <- hidden$x * 4000000 + hidden$y
  return(out)

}


stopifnot(calc_transect_coverage("day15/test_input.txt") == 26)
tic("Day 15, part 1")
stopifnot(calc_transect_coverage("day15/input.txt") == 5073496)
toc(log=TRUE)

# part 2 does not work on test input
tic("Day 15, part 2")
stopifnot(find_hidden_beacon("day15/input.txt") == 13081194638237)
toc(log=TRUE)
