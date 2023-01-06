
score_completion_patterns <- function(patterns) {
  dict <- c(")", "]", "}", ">")
  out <- rep(NA, length(patterns))
  for (i in 1:length(patterns)) {
    dat <- strsplit(patterns[i], split = "")[[1]]
    scores <- match(dat, dict)
    total_score <- 0
    for (j in 1:length(scores)) {
      total_score <- 5 * total_score + scores[j]
    }
    out[i] <- total_score
  }
  return(out)
}

analyze_lines <- function(path) {
  raw <- readLines(path)
  dict <- data.frame(
    start = c("(", "[", "{", "<"),
    stop = c(")", "]", "}", ">"),
    corruption_score = c(3, 57, 1197, 25137)
  )
  out <- data.frame(
    pattern = raw,
    incomplete = NA,
    corrupt = NA,
    corruption_score = NA,
    patch = NA,
    patch_score = NA
  )
  for (i in 1:nrow(out)) {
    dat <- strsplit(out$pattern[i], split = "")[[1]]
    current_chunk <- ""
    corrupt <- rep(FALSE, length(dat))
    for (j in 1:length(dat)) {
      if (dat[j] %in% dict$start) {
        current_chunk <- c(current_chunk, dict$stop[match(dat[j], dict$start)])
      }
      if (dat[j] %in% dict$stop) {
        corrupt[j] <- current_chunk[length(current_chunk)] != dat[j]
        if (!corrupt[j]) current_chunk <- current_chunk[-length(current_chunk)]
      }
    }
    if (any(corrupt)) {
      out$corrupt[i] <- TRUE
      first_corrupt <- dat[min(which(corrupt))]
      out$corruption_score[i] <- dict$corruption_score[match(first_corrupt, dict$stop)]
    } else if (length(current_chunk) > 1) {
      out$incomplete[i] <- TRUE
      out$patch[i] <- paste(rev(current_chunk), collapse = "")
      out$patch_score[i] <- score_completion_patterns(out$patch[i])
    } else {
      out$incomplete[i] <- FALSE
    }
  }
  return(out)
}

d_test <- analyze_lines("day10/test_input.txt")

tic("day 10, part 1 and 2")
d <- analyze_lines("day10/input.txt")
toc(log = TRUE)

# part 1
stopifnot(sum(d_test$corruption_score, na.rm = TRUE) == 26397)
stopifnot(sum(d$corruption_score, na.rm = TRUE) == 216297)

# part 2
stopifnot(median(d_test$patch_score, na.rm = TRUE) == 288957)
stopifnot(median(d$patch_score, na.rm = TRUE) == 2165057169)
