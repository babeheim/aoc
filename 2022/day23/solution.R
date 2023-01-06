
library(tictoc)

# convert from (x,y) to index
con_yx_i <- function(y, x, n) {
  (x - 1) * n + y
}
con_i_xy <- function(i, n) {
  data.frame(
    y = (i - 1) %% n + 1,
    x = (i - 1) %/% n + 1
  )
}


sim_elf_diffusion <- function(path, viz = FALSE, calibration = FALSE) {

  x <- readLines(path)
  buffer <- 60
  map <- matrix(FALSE, ncol = nchar(x) + buffer * 2, nrow = length(x) + buffer * 2)

  for (i in 1:length(x)) {
    for (j in 1:nchar(x[i])) {
      if (substr(x[i], j, j) == "#") {
        map[i + buffer, j + buffer] <- TRUE
      }
    }
  }

  elves <- data.frame(
    i = which(map),
    y = which(map, arr.ind = TRUE)[,"row"],
    x = which(map, arr.ind = TRUE)[,"col"]
  )

  check_seq <- c("E", "N", "S", "W") # order before first round

  view <- c(-1, 0, 1)

  # begin simulation

  rounds_finished <- 0
  sim_finished <- FALSE

  while (!sim_finished) {
  
    # init round
    elves$prop_i <- NA
    elves$solo <- NA
    elves$report <- ""
    check_seq <- c(check_seq[2:4], check_seq[1])

    # solo check
    for (i in 1:nrow(elves)) {
      elves$solo[i] <- sum(map[elves$y[i] + view, elves$x[i] + view]) == 1
    }

    # for those elves who arent solo, run through the proposal check_seq
    for (check in check_seq) {
      active <- which(!elves$solo & is.na(elves$prop_i))
      for (i in seq_along(active)) {
        # check north
        if (check == "N") {
          region_clear <- !any(map[elves$y[active[i]] - 1, elves$x[active[i]] + view])
          if (region_clear) {
            elves$prop_i[active[i]] <- con_yx_i(elves$y[active[i]] - 1, elves$x[active[i]], nrow(map))
            elves$report[active[i]] <- "proposing North"
          } else {
            elves$report[active[i]] <- "North occupied"
          }
        }
        if (check == "S") {
          region_clear <- !any(map[elves$y[active[i]] + 1, elves$x[active[i]] + view])
          if (region_clear) {
            elves$prop_i[active[i]] <- con_yx_i(elves$y[active[i]] + 1, elves$x[active[i]], nrow(map))
            elves$report[active[i]] <- "proposing South"
          } else {
            elves$report[active[i]] <- "South occupied"
          }
        }
        if (check == "E") {
          region_clear <- !any(map[elves$y[active[i]] + view, elves$x[active[i]] + 1])
          if (region_clear) {
            elves$prop_i[active[i]] <- con_yx_i(elves$y[active[i]], elves$x[active[i]] + 1, nrow(map))
            elves$report[active[i]] <- "proposing East"
          } else {
            elves$report[active[i]] <- "East occupied"
          }
        }
        if (check == "W") {
          region_clear <- !any(map[elves$y[active[i]] + view, elves$x[active[i]] - 1])
          if (region_clear) {
            elves$prop_i[active[i]] <- con_yx_i(elves$y[active[i]], elves$x[active[i]] - 1, nrow(map))
            elves$report[active[i]] <- "proposing West"
          } else {
            elves$report[active[i]] <- "West occupied"
          }
        }
      }
    }

    elves$conflicted <- elves$prop_i %in% elves$prop_i[which(!is.na(elves$prop_i) & duplicated(elves$prop_i))]
    if (any(elves$conflicted)) {
      tar <- which(elves$conflicted)
      elves$report[tar] <- "conflicting proposals; staying put"
    }

    if (viz) {
      plot(NULL, xlim = c(1, ncol(map)), ylim = c(nrow(map), 1), axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "")
      tar <- which(elves$solo | is.na(elves$prop_i))
      points(elves$x[tar], elves$y[tar], pch = 16, col = "black", cex = 0.5)
      tar <- grep("conflicting", elves$report)
      points(elves$x[tar], elves$y[tar], pch = 16, col = "red", cex = 0.5)
      tar <- grep("proposing", elves$report)
      points(elves$x[tar], elves$y[tar], pch = 16, col = "blue", cex = 0.5)
      # for (i in 1:nrow(elves)) {
      #   if (!is.na(elves$prop_i[i])) {
      #     dest <- con_i_xy(elves$prop_i[i], nrow(map))
      #     points(dest$x, dest$y, col = ifelse(elves$conflicted[i], "red", "blue"))
      #     lines(c(elves$x[i], dest$x), c(elves$y[i], dest$y), col = ifelse(elves$conflicted[i], "red", "blue"))
      #   }
      # }
    }

    # finally, active elves without conflicts move to proposal spots
    map[elves$i] <- FALSE
    moving <- which(!is.na(elves$prop_i) & !elves$conflicted)
    if (length(moving) > 0) {
      elves$i[moving] <- elves$prop_i[moving]
      elves$x[moving] <- con_i_xy(elves$i[moving], nrow(map))$x
      elves$y[moving] <- con_i_xy(elves$i[moving], nrow(map))$y
    }
    map[elves$i] <- TRUE

    rounds_finished <- rounds_finished + 1L
    sim_finished <- (calibration & rounds_finished == 10L) | length(moving) == 0

  }

  if (calibration) {
    # number of open spaces:
    out <- ((diff(range(elves$x))) + 1) * (diff(range(elves$y)) + 1) - nrow(elves)
  } else {
    # number of completed rounds
    out <- rounds_finished
  }
  return(out)

}


stopifnot(sim_elf_diffusion("day23/test_input.txt", calibration = TRUE) == 110)

tic("Day 23, part 1")
stopifnot(sim_elf_diffusion("day23/input.txt", calibration = TRUE) == 3766)
toc(log=TRUE)

stopifnot(sim_elf_diffusion("day23/test_input.txt") == 20)

tic("Day 23, part 2")
stopifnot(sim_elf_diffusion("day23/input.txt") == 954)
toc(log=TRUE)
