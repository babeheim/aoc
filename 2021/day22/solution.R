
calc_reactor_volume <- function(path, init_stage = TRUE, verbose = FALSE) {

  raw <- readLines(path)

  op <- data.frame(
    turn_on = logical(),
    xmin = character(),
    xmax = character(),
    ymin = character(),
    ymax = character(),
    zmin = character(),
    zmax = character(),
    reactor_delta = numeric()
  )

  for (i in seq_along(raw)) {
    add <- list()
    add$turn_on <- grepl("^on\\s", raw[i])
    raw_i <- substr(raw[i], as.numeric(gregexpr("\\s", raw[i])) + 1, nchar(raw[i]))
    raw_i_vec <- strsplit(raw_i, ",")[[1]]
    add$xmin <- substr(raw_i_vec[1], regexpr("x=", raw_i_vec[1]) + 2, regexpr("\\.\\.", raw_i_vec[1])-1)
    add$xmax <- substr(raw_i_vec[1], regexpr("\\.\\.", raw_i_vec[1]) + 2, nchar(raw_i_vec[1]))

    add$ymin <- substr(raw_i_vec[2], regexpr("y=", raw_i_vec[2]) + 2, regexpr("\\.\\.", raw_i_vec[2]) - 1)
    add$ymax <- substr(raw_i_vec[2], regexpr("\\.\\.", raw_i_vec[2]) + 2, nchar(raw_i_vec[2]))

    add$zmin <- substr(raw_i_vec[3], regexpr("z=", raw_i_vec[3]) + 2, regexpr("\\.\\.", raw_i_vec[3]) - 1)
    add$zmax <- substr(raw_i_vec[3], regexpr("\\.\\.", raw_i_vec[3]) + 2, nchar(raw_i_vec[3]))
    op <- dplyr::bind_rows(op, add)
  }

  op$xmin <- as.numeric(op$xmin)
  op$xmax <- as.numeric(op$xmax)
  op$ymin <- as.numeric(op$ymin)
  op$ymax <- as.numeric(op$ymax)
  op$zmin <- as.numeric(op$zmin)
  op$zmax <- as.numeric(op$zmax)

  rctr <- list(
    xmin = min(op$xmin),
    xmax = max(op$xmax),
    ymin = min(op$ymin),
    ymax = max(op$ymax),
    zmin = min(op$zmin),
    zmax = max(op$zmax)
  )

  if (init_stage) {
    # truncate shapes that go over the boundaries
    op$xmin[op$xmin < -50] <- (-50)
    op$ymin[op$ymin < -50] <- (-50)
    op$zmin[op$zmin < -50] <- (-50)
    op$xmax[op$xmax > 50] <- (50)
    op$ymax[op$ymax > 50] <- (50)
    op$zmax[op$zmax > 50] <- (50)

    # cut off shapes totally outside the range
    op$xmax[op$xmax < -50] <- NA
    op$ymax[op$ymax < -50] <- NA
    op$zmax[op$zmax < -50] <- NA
    op$xmin[op$xmin > 50] <- NA
    op$ymin[op$ymin > 50] <- NA
    op$zmin[op$zmin > 50] <- NA
  }

  op$xlen <- (op$xmax - op$xmin) + 1
  op$ylen <- (op$ymax - op$ymin) + 1
  op$zlen <- (op$zmax - op$zmin) + 1

  op$volume <- op$xlen * op$ylen * op$zlen

  op$xmin_j <- op$xmax_j <- NA
  op$ymin_j <- op$ymax_j <- NA
  op$zmin_j <- op$zmax_j <- NA
  op$xlen_j <- op$ylen_j <- op$zlen_j <- NA

  drop <- which(is.na(op$xlen) | is.na(op$ylen) | is.na(op$zlen))
  if (length(drop) > 0) op <- op[-drop, , drop = FALSE]

  ########

  for (j in seq_len(nrow(op))) {
    if (verbose) print(paste("evaluating box", j))
    op$reactor_delta[j] <- 0
    if (op$turn_on[j]) {
      jth_volume <- op$xlen[j] * op$ylen[j] * op$zlen[j]
      jth_overlap <- 0
      if (j != nrow(op)) {
        # truncate all dimensions for subsequent boxes
        op$xmin_j <- pmax(op$xmin, op$xmin[j])
        op$xmax_j <- pmin(op$xmax, op$xmax[j])
        op$ymin_j <- pmax(op$ymin, op$ymin[j])
        op$ymax_j <- pmin(op$ymax, op$ymax[j])
        op$zmin_j <- pmax(op$zmin, op$zmin[j])
        op$zmax_j <- pmin(op$zmax, op$zmax[j])
        # use inclusion-exclusion to sum all volumes subsequent to the jth box, within the jth volume
        # need to subset here to just those boxes within the boundaries...
        op$xlen_j <- op$xmax_j - op$xmin_j + 1
        op$ylen_j <- op$ymax_j - op$ymin_j + 1
        op$zlen_j <- op$zmax_j - op$zmin_j + 1
        remaining <- which(seq_len(nrow(op)) > j & op$xlen_j > 0 & op$ylen_j > 0 & op$zlen_j > 0)
        n_boxes <- length(remaining)
        if (n_boxes > 0) {
          if (verbose) print(paste(n_boxes, "boxes remaining to consider overlaps"))
          combos <- expand.grid(rep(list(c(FALSE, TRUE)), n_boxes)) # there are too many combinations...
          combos <- combos[-1,,drop = FALSE]
          for (i in seq_len(nrow(combos))) {
            this_combo <- remaining[which(as.logical(combos[i,]))]
            if (verbose) print(paste("considering boxes", paste(this_combo, collapse = ", ")))
            x_overlapping <- min(op$xmax_j[this_combo]) >= max(op$xmin_j[this_combo])
            y_overlapping <- min(op$ymax_j[this_combo]) >= max(op$ymin_j[this_combo])
            z_overlapping <- min(op$zmax_j[this_combo]) >= max(op$zmin_j[this_combo])
            if (x_overlapping & y_overlapping & z_overlapping) {
              x_len <- min(op$xmax_j[this_combo]) - max(op$xmin_j[this_combo]) + 1
              y_len <- min(op$ymax_j[this_combo]) - max(op$ymin_j[this_combo]) + 1
              z_len <- min(op$zmax_j[this_combo]) - max(op$zmin_j[this_combo]) + 1
              v_overlap <- x_len * y_len * z_len
              if (verbose) print(paste("added overlap of", (-1)^(length(this_combo) - 1) * v_overlap))
              jth_overlap <- jth_overlap + (-1)^(length(this_combo) - 1) * v_overlap
              stopifnot(!is.na(jth_overlap))
            }
          }
        }
      }
      if (verbose) print(paste("total volume is", jth_volume))
      if (verbose) print(paste("of that,", jth_overlap, "is overlapping with subsequent boxes"))
      op$reactor_delta[j] <- jth_volume - jth_overlap
      if (verbose) print(paste("so reactor_delta[j] is", op$reactor_delta[j]))
    }
  }

  return(sum(op$reactor_delta))

}

stopifnot(calc_reactor_volume("day22/test_input.txt") == 39)
stopifnot(calc_reactor_volume("day22/test_input2.txt") == 590784)
stopifnot(calc_reactor_volume("day22/test_input3.txt") == 474140)

tic("day 22, part 1")
stopifnot(calc_reactor_volume("day22/input.txt") == 596989)
toc(log = TRUE)

stopifnot(calc_reactor_volume("day22/test_input3.txt", init_stage = FALSE) == 2758514936282235)

tic("day 22, part 2")
stopifnot(calc_reactor_volume("day22/input.txt", init_stage = FALSE) == 1160011199157381)
toc(log = TRUE)
