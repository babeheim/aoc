
count_elements <- function(elements, pairs, pairs_n, x_init) {
  n <- rep(0, nrow(elements))
  for (i in 1:nrow(pairs)) {
    tar <- which(elements$element == substr(pairs$pair[i], 1, 1))
    n[tar] <- n[tar] + pairs_n[i]
    tar <- which(elements$element == substr(pairs$pair[i], 2, 2))
    n[tar] <- n[tar] + pairs_n[i]
  }
  n[which(elements$element == x_init[1])] <- n[which(elements$element == x_init[1])] + 1
  n[which(elements$element == x_init[length(x_init)])] <- n[which(elements$element == x_init[length(x_init)])] + 1
  n <- round(n / 2, 1)
  return(n)
}

tabulate_elements <- function(path) {
  raw <- readLines(path)
  x <- strsplit(raw[1], "")[[1]]
  raw <- raw[3:length(raw)]
  pairs <- data.frame(
    pair = substr(raw, 1, regexpr("\\s", raw) - 1),
    child = substr(raw, regexpr("->\\s", raw) + 3, nchar(raw))
  )
  A <- matrix(0, nrow = nrow(pairs), ncol = nrow(pairs))
  for (i in 1:nrow(pairs)) {
    child_pair_1 <- paste0(substr(pairs$pair[i], 1, 1), pairs$child[i])
    child_pair_2 <- paste0(pairs$child[i], substr(pairs$pair[i], 2, 2))
    # for each pair as a column, flag rows which are children pairs
    A[which(pairs$pair %in% c(child_pair_1, child_pair_2)), i] <- 1
  }
  # tabulate pairs in initial chain x
  pairs$n_initial <- 0
  for (i in 1:(length(x) - 1)) {
    tar <- which(pairs$pair == paste(x[i:(i + 1)], collapse = ""))
    pairs$n_initial[tar] <- pairs$n_initial[tar] + 1
  }
  # A is singular, so gotta matrix-multiply in a for-loop
  n_running <- pairs$n_initial
  for (i in 1:40) {
    n_running <- A %*% n_running
    if (i == 1) pairs$n_after_1 <- n_running
    if (i == 10) pairs$n_after_10 <- n_running
    if (i == 40) pairs$n_after_40 <- n_running
  }
  # now just count how many elements we have
  elements <- data.frame(
    element = sort(unique(pairs$child))
  )
  elements$n_initial <- 0
  for (i in 1:length(x)) {
    elements$n_initial[which(elements$element == x[i])] <- elements$n_initial[which(elements$element == x[i])] + 1
  }
  elements$n_after_1 <- count_elements(elements, pairs, pairs$n_after_1, x)
  elements$n_after_10 <- count_elements(elements, pairs, pairs$n_after_10, x)
  elements$n_after_40 <- count_elements(elements, pairs, pairs$n_after_40, x)
  return(elements)
}

tab <- tabulate_elements("day14/test_input.txt")
max(tab$n_after_10) - min(tab$n_after_10) == 1588
max(tab$n_after_40) == 2192039569602
min(tab$n_after_40) == 3849876073
max(tab$n_after_40) - min(tab$n_after_40) == 2188189693529

tic("day 14, parts 1 and 2")
tab <- tabulate_elements("day14/input.txt")
toc(log = TRUE)

stopifnot(max(tab$n_after_10) - min(tab$n_after_10) == 2549) # part 1 answer

stopifnot(max(tab$n_after_40) - min(tab$n_after_40) == 2516901104210) # part 2 answer
