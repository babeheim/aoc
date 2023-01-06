
library(tictoc)

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

  inst$src <- number_scraper("(?<=from )\\d+", inst$text)
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
        col_len <- length(stacks[[inst$src[i]]])
        stopifnot(col_len > 0)
        stacks[[inst$dest[i]]] <- c(stacks[[inst$dest[i]]], stacks[[inst$src[i]]][col_len])
        stacks[[inst$src[i]]] <- stacks[[inst$src[i]]][-col_len]
      }
    } else if (move_type == "multiple at once") {
      col_len <- length(stacks[[inst$src[i]]])
      stopifnot(col_len > 0)
      targets <- (col_len - inst$n_moves[i] + 1):col_len
      stacks[[inst$dest[i]]] <- c(stacks[[inst$dest[i]]], stacks[[inst$src[i]]][targets])
        stacks[[inst$src[i]]] <- stacks[[inst$src[i]]][-targets]

    } else stop("invalid move_type")
  }

  top_box <- rep(NA, n_cols)
  for (i in seq_len(n_cols)) {
    top_box[i] <- stacks[[i]][length(stacks[[i]])]
  }
  out <- paste(top_box, collapse = "")
  return(out)
}

stopifnot(find_top_crates("day05/test_input.txt") == "CMZ")
tic("Day 05, part 1")
stopifnot(find_top_crates("day05/input.txt") == "CWMTGHBDW")
toc(log=TRUE)

stopifnot(find_top_crates("day05/test_input.txt", move_type = "multiple at once") == "MCD")
tic("Day 05, part 2")
stopifnot(find_top_crates("day05/input.txt", move_type = "multiple at once") == "SSCGWJCRB")
toc(log=TRUE)

