
library(tictoc)

number_scraper <- function(pattern, x) {
  m <- regexpr(pattern, x, perl = TRUE)
  as.integer(regmatches(x, m))
}

calc_max_output <- function(x, n_minutes = 24, verbose = FALSE) {

  ore_robot_ore_price <- number_scraper("(?<=Each ore robot costs\\s)\\d+", x)
  clay_robot_ore_price <- number_scraper("(?<=Each clay robot costs\\s)\\d+", x)
  obsidian_robot_ore_price <- number_scraper("(?<=Each obsidian robot costs\\s)\\d+", x)
  geode_robot_ore_price <- number_scraper("(?<=Each geode robot costs\\s)\\d+", x)

  obsidian_robot_clay_price <- number_scraper("(?<=and\\s)\\d+(?=\\sclay)", x)
  geode_robot_obsidian_price <- number_scraper("(?<=and\\s)\\d+(?=\\sobsidian)", x)

  max_ore_robots <- max(c(ore_robot_ore_price, clay_robot_ore_price, obsidian_robot_ore_price, geode_robot_ore_price))
  max_clay_robots <- obsidian_robot_clay_price
  max_obsidian_robots <- geode_robot_obsidian_price

  running_max <- 0

  root <- list()
  root$minute <- 1
  root$n_ores <- 1
  root$n_ore_robots <- 1
  root$n_clays <- 0
  root$n_clay_robots <- 0
  root$n_obsidians <- 0
  root$n_obsidian_robots <- 0
  root$n_geodes <- 0
  root$n_geode_robots <- 0

  node_counter <- 1L

  stack <- list(root)

  while (length(stack) > 0) {

    if (verbose) {
      if (node_counter %% 10000L == 0) print(node_counter)
      node_counter <- node_counter + 1L
    }
    
    current <- stack[[length(stack)]]
    stack <- stack[-length(stack)]

    running_max <- max(running_max, current$n_geodes)
    total_time_remaining <- (n_minutes - current$minute)

    # given your current state, if you could produce geode robot every time period from this point forwards, what would you get?
    current_theoretical_geode_limit <- current$n_geodes + total_time_remaining * current$n_geode_robots + total_time_remaining * (total_time_remaining + 1) / 2
    # if your current running_max is bigger than this, implies this branch is worth pruning!

    if (total_time_remaining > 0 & running_max <= current_theoretical_geode_limit) {

      # consider a future in which we build another ore robot, if possible
      harvest_time_for_ore_robot <- ceiling(max((ore_robot_ore_price - current$n_ores) / current$n_ore_robots, 0))
      total_time_for_ore_robot <- harvest_time_for_ore_robot + 1
      another_ore_robot_buildable <- (current$minute + total_time_for_ore_robot) < n_minutes
      if (another_ore_robot_buildable & current$n_ore_robots < max_ore_robots) {
        future <- current
        future$minute <- current$minute + total_time_for_ore_robot
        future$n_ores <- current$n_ores + current$n_ore_robots * total_time_for_ore_robot - ore_robot_ore_price
        future$n_clays <- current$n_clays + current$n_clay_robots * total_time_for_ore_robot
          if (future$n_clays < 0) stop("ore")
        future$n_obsidians <- current$n_obsidians + current$n_obsidian_robots * total_time_for_ore_robot
        future$n_geodes <- current$n_geodes + current$n_geode_robots * total_time_for_ore_robot
        future$n_ore_robots <- current$n_ore_robots + 1
        stack[[length(stack)+1]] <- future
      }

      # consider a future in which we build another clay robot, if possible
      harvest_time_for_clay_robot <- ceiling(max((clay_robot_ore_price - current$n_ores) / current$n_ore_robots, 0))
      total_time_for_clay_robot <- harvest_time_for_clay_robot + 1
      another_clay_robot_buildable <- (current$minute + total_time_for_clay_robot) < n_minutes
      if (another_clay_robot_buildable & current$n_clay_robots < max_clay_robots) {
        future <- current
        future$minute <- current$minute + total_time_for_clay_robot
        future$n_ores <- current$n_ores + current$n_ore_robots * total_time_for_clay_robot - clay_robot_ore_price
        future$n_clays <- current$n_clays + current$n_clay_robots * total_time_for_clay_robot
        future$n_obsidians <- current$n_obsidians + current$n_obsidian_robots * total_time_for_clay_robot
        future$n_geodes <- current$n_geodes + current$n_geode_robots * total_time_for_clay_robot
        future$n_clay_robots <- current$n_clay_robots + 1
        stack[[length(stack)+1]] <- future
      }

      # consider a future in which we build another obsidian robot, if possible
      if (current$n_clay_robots > 0) {
        harvest_time_for_obsidian_robot <- max(
          ceiling((obsidian_robot_ore_price - current$n_ores) / current$n_ore_robots),
          ceiling((obsidian_robot_clay_price - current$n_clays) / current$n_clay_robots),
          0
        )
        total_time_for_obsidian_robot <- harvest_time_for_obsidian_robot + 1
        another_obsidian_robot_buildable <- (current$minute + total_time_for_obsidian_robot) < n_minutes
        if (another_obsidian_robot_buildable & current$n_obsidian_robots < max_obsidian_robots) {
          future <- current
          future$minute <- current$minute + total_time_for_obsidian_robot
          future$n_ores <- current$n_ores + current$n_ore_robots * total_time_for_obsidian_robot - obsidian_robot_ore_price
          future$n_clays <- current$n_clays + current$n_clay_robots * total_time_for_obsidian_robot - obsidian_robot_clay_price
          future$n_obsidians <- current$n_obsidians + current$n_obsidian_robots * total_time_for_obsidian_robot
          future$n_geodes <- current$n_geodes + current$n_geode_robots * total_time_for_obsidian_robot
          future$n_obsidian_robots <- current$n_obsidian_robots + 1
          stack[[length(stack)+1]] <- future
        }
      }

      # consider a future in which we build another geode robot, if possible
      if (current$n_obsidian_robots > 0) {
        harvest_time_for_geode_robot <- max(
          ceiling((geode_robot_ore_price - current$n_ores) / current$n_ore_robots),
          ceiling((geode_robot_obsidian_price - current$n_obsidians) / current$n_obsidian_robots),
          0
        )
        total_time_for_geode_robot <- harvest_time_for_geode_robot + 1
        another_geode_robot_buildable <- (current$minute + total_time_for_geode_robot) < n_minutes
        if (another_geode_robot_buildable) {
          future <- current
          future$minute <- current$minute + total_time_for_geode_robot
          future$n_ores <- current$n_ores + current$n_ore_robots * total_time_for_geode_robot - geode_robot_ore_price
          future$n_clays <- current$n_clays + current$n_clay_robots * total_time_for_geode_robot
          future$n_obsidians <- current$n_obsidians + current$n_obsidian_robots * total_time_for_geode_robot - geode_robot_obsidian_price
          future$n_geodes <- current$n_geodes + current$n_geode_robots * total_time_for_geode_robot
          future$n_geode_robots <- current$n_geode_robots + 1
          stack[[length(stack)+1]] <- future
        }
      }

      # always consider a future of just idling
      final <- current
      final$minute <- n_minutes
      final$n_ores <- current$n_ores + current$n_ore_robots * total_time_remaining
      final$n_clays <- current$n_clays + current$n_clay_robots * total_time_remaining
      final$n_obsidians <- current$n_obsidians + current$n_obsidian_robots * total_time_remaining
      final$n_geodes <- current$n_geodes + current$n_geode_robots * total_time_remaining
      stack[[length(stack)+1]] <- final

    }
  }

  return(running_max)

}

score_strategies <- function(path, part2 = FALSE, verbose = FALSE) {
  x <- readLines(path)
  if (part2) {
    x <- x[1:3] # for part 2!
    n_minutes <- 32
  } else {
    n_minutes <- 24
  }
  scores <- rep(NA, length(x))
  for (i in 1:length(x)) {
    scores[i] <- calc_max_output(x[i], n_minutes = n_minutes, verbose = verbose)
    if (verbose) cat("scored line", i, "\n")
  }
  if (part2) {
    out <- prod(scores)
  } else {
    out <- sum(1:length(scores) * scores)
  }
  return(out)
}

stopifnot(score_strategies("day19/test_input.txt") == 33)

tic("day 19, part 1")
stopifnot(score_strategies("day19/input.txt") == 1487)
toc(log = TRUE)

tic("day 19, part 2")
stopifnot(score_strategies("day19/input.txt", part2 = TRUE) == 13440)
toc(log = TRUE)
