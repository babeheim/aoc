
library(tictoc)
library(jsonlite)

compare <- function(l, r, verbose = FALSE) {
  if (is.integer(l) & is.integer(r)) {
    # If both values are integers, the lower integer should come first. If the left integer is lower than the right integer, the inputs are in the right order. If the left integer is higher than the right integer, the inputs are not in the right order. Otherwise, the inputs are the same integer; continue checking the next part of the input.
    if (verbose) cat("compare", l, "vs", r, "\n")
    if (l < r) {
      if (verbose) cat("Left side is smaller, so inputs are in the right order\n")
      return("good - l < r")
    }
    if (l > r) {
      if (verbose) cat("Right side is smaller, so inputs are not in the right order\n")
      return("wrong - l > r")
    }
    if (r == l) {
      return("continue")
    }
  }
  if (is.list(l) & is.list(r)) {
    # left side 
    if (verbose) cat("Compare", as.character(toJSON(l)), "vs", as.character(toJSON(r)), "\n")
    if (length(l) > 0 & length(r) == 0) {
      # "If the right list runs out of items first, the inputs are not in the right order."
      if (verbose) cat("Right side ran out of items, so inputs are not in the right order\n")
      return("wrong - right side ran out of items before left side")
    }
    if (length(l) == 0 & length(r) > 0) {
      # "...If the left list runs out of items first, the inputs are in the right order."
      if (verbose) cat("Left side ran out of items, so inputs are in the right order\n")
      return("good - left side is empty and right side is not")
    }
    for (i in seq_len(length(l))) {
      # "If both values are lists, compare the first value of each list, then the second value, and so on..."
      out <- compare(l[[i]], r[[i]])
      if (out != "continue") return(out)

      if (i == length(r) & length(l) > length(r)) {
        # "If the right list runs out of items first, the inputs are not in the right order."
        if (verbose) cat("Right side ran out of items, so inputs are not in the right order\n")
        return("wrong - right side ran out of items before left side")
      }
      if (i == length(l) & length(l) < length(r)) {
        # "...If the left list runs out of items first, the inputs are in the right order."
        if (verbose) cat("Left side ran out of items, so inputs are in the right order\n")
        return("good - left side ran out of items before right side")
      }
    }
    return("continue")
  }
  if (!is.list(l) & is.list(r)) {
    # ...convert the integer to a list which contains that integer as its only value, then retry the comparison. 
    if (verbose) {
      cat("Compare", l, "vs", as.character(toJSON(r)), "\n")
      cat("Mixed types; convert left to", as.character(toJSON(as.list(l))), "and retry comparison\n")
    }
    out <- compare(as.list(l), r)
    return(out)
  }
  if (is.list(l) & !is.list(r)) {
    # ...convert the integer to a list which contains that integer as its only value, then retry the comparison. 
    if (verbose) {
      cat("Compare", as.character(toJSON(l)), "vs", r, "\n")
      cat("Mixed types; convert right to", as.character(toJSON(as.list(r))), "and retry comparison\n")
    }
    out <- compare(l, as.list(r))
    return(out)
  }
}


find_bad_pairs <- function(path) {
  x <- readLines(path)
  pair_breaks <- c(which(x == ""), length(x) + 1)
  verdicts <- rep(NA, length(pair_breaks))
  for (i in seq_along(pair_breaks)) {
    l <- fromJSON(x[pair_breaks[i] - 2], simplifyVector = FALSE)
    r <- fromJSON(x[pair_breaks[i] - 1], simplifyVector = FALSE)
    verdicts[i] <- compare(l, r)
  }
  out <- sum(grep("^good", verdicts))
  return(out)
}


swap <- function(i, j, x) {
  hold <- x[j]
  x[j] <- x[i]
  x[i] <- hold
  return(x)
}

bubble_sort <- function(x) {
  any_swapped <- TRUE
  while (any_swapped) {
    any_swapped <- FALSE
    for (i in 2:length(x)) {
      l <- fromJSON(x[i-1], simplifyVector = FALSE)
      r <- fromJSON(x[i], simplifyVector = FALSE)
      verdict <- compare(l, r)
      if (grepl("wrong", verdict)) {
        x <- swap(i-1, i, x)
        any_swapped <- TRUE
      }
    }
  }
  return(x)
}

sort_codes <- function(path) {
  x <- readLines(path)
  x <- x[-which(x == "")]
  divider_packets <- c("[[2]]", "[[6]]")
  x <- c(x, divider_packets)
  x <- bubble_sort(x)
  out <- prod(which(x %in% divider_packets))
  return(out)
}


stopifnot(find_bad_pairs("day13/test_input.txt") == 13)
stopifnot(find_bad_pairs("day13/test_input_ii.txt") == 4643)
tic("Day 13, part 1")
stopifnot(find_bad_pairs("day13/input.txt") == 5720)
toc(log=TRUE)

stopifnot(sort_codes("day13/test_input.txt") == 140)

tic("Day 13, part 2")
stopifnot(sort_codes("day13/input.txt") == 23504)
toc(log=TRUE)
