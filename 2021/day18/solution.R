
snail_to_list <- function(chr) {
  if (length(chr) == 1) {
    chr <- gsub("\\[", "list(", chr)
    chr <- gsub("\\]", ")", chr)
    out <- eval(parse(text = chr))
  } else {
    out <- lapply(chr, snail_to_list)
  }
  return(out)
}

split <- function(value) {
  list(floor(value/2), ceiling(value/2))
}

split_tree <- function(tree) {
  for (i in 1:2) {
    if (is.numeric(tree[[i]])) {
      if (tree[[i]] > 9) {
        tree[[i]] <- split(tree[[i]])
      }
    } else {
      tree[[i]] <- split_tree(tree[[i]])
    }
  }
  return(tree)
}

split_once <- function(tree, has_split = FALSE) {
  for (i in 1:2) {
    if (is.numeric(tree[[i]])) {
      if (!has_split & tree[[i]] > 9) {
        tree[[i]] <- split(tree[[i]])
        has_split <- TRUE
      }
    } else {
      recurse <- split_once(tree[[i]], has_split)
      tree[[i]] <- recurse$tree
      has_split <- recurse$has_split
    }
  }
  out <- list(tree = tree, has_split = has_split)
  return(out)
}

count_tree <- function(tree, counter = 0) {
  for (i in 1:2) {
    if (is.numeric(tree[[i]])) {
      counter <- counter + 1
    } else {
      counter <- count_tree(tree[[i]], counter)
    }
  }
  return(counter)
}

# how to add values at specific locations
add_value <- function(tree, target, value, counter = 1) {
  binary_order <- 1:2
  for (i in binary_order) {
    if (is.numeric(tree[[i]])) {
      if (counter == target) {
        tree[[i]] <- tree[[i]] + value
      }
      counter <- counter + 1
    } else {
      recurse <- add_value(tree[[i]], target, value, counter)
      tree[[i]] <- recurse$tree
      counter <- recurse$counter
    }
  }
  return(list(tree = tree, counter = counter)) 
}

explode_tree <- function(tree) {
  n_values <- count_tree(tree)
  counter <- 0
  for (d1 in 1:2) {
    if (is.numeric(tree[[d1]])) {
      counter <- counter + 1
    } else if (is.list(tree[[d1]])) {
      for (d2 in 1:2) {
        if (is.numeric(tree[[d1]][[d2]])) {
          counter <- counter + 1
        } else if (is.list(tree[[d1]][[d2]])) {
          for (d3 in 1:2) {
            if (is.numeric(tree[[d1]][[d2]][[d3]])) {
              counter <- counter + 1
            } else if (is.list(tree[[d1]][[d2]][[d3]])) {
              for (d4 in 1:2) {
                if (is.numeric(tree[[d1]][[d2]][[d3]][[d4]])) {
                  counter <- counter + 1
                } else if (is.list(tree[[d1]][[d2]][[d3]][[d4]])) {
                  # this needs to be exploded!
                  left_value <- tree[[d1]][[d2]][[d3]][[d4]][[1]]
                  left_index <- counter + 1
                  if (1 <= (left_index - 1)) {
                    tree <- add_value(tree, left_index - 1, left_value)$tree
                  }
                  right_value <- tree[[d1]][[d2]][[d3]][[d4]][[2]]
                  right_index <- counter + 2
                  if ((right_index + 1) <= n_values) {
                    tree <- add_value(tree, right_index + 1, right_value)$tree
                  }
                  # having added the values to adjacent entries, explode the node
                  tree[[d1]][[d2]][[d3]][[d4]] <- 0
                  counter <- counter + 1
                }
              }
            }
          }
        }
      }
    }
  }
  return(tree)
}

calc_magnitude <- function(tree) {
  if (is.numeric(tree[[1]])) {
    left <- tree[[1]]
  } else {
    left <- calc_magnitude(tree[[1]])
  }
  if (is.numeric(tree[[2]])) {
    right <- tree[[2]]
  } else {
    right <- calc_magnitude(tree[[2]])
  }
  out <- 3 * left + 2 * right
  return(out)
}



reduce_tree <- function(tree) {
  check <- TRUE
  while (check) {
    # explode all pairs nested 4 levels down
    tree <- explode_tree(tree)
    # split all values that are > 9 into pairs
    split_tree <- split_once(tree)$tree
    if (!identical(split_tree, tree)) {
      tree <- split_tree
    } else {
      check <- FALSE
    }
    # once a single split has been made, we have to check whether further explosions are required
  }
  return(tree)
}


add_snail_numbers <- function(snail_list, path = NULL) {
  if (!is.null(path)) snail_list <- snail_to_list(readLines(path))
  out <- snail_list[[1]]
  for (i in 2:length(snail_list)) {
    out <- reduce_tree(list(out, snail_list[[i]]))
  }
  return(out)
}


stopifnot(calc_magnitude(add_snail_numbers(path = "day18/test_input.txt")) == 4140)

tic("day 18, part 1")
stopifnot(calc_magnitude(add_snail_numbers(path = "day18/input.txt")) == 4120) # part 1
toc(log = TRUE)


calc_max_magnitude <- function(path, verbose = FALSE) {
  snail_numbers <- snail_to_list(readLines(path))
  grid <- expand.grid(n1 = seq_len(length(snail_numbers)), n2 = seq_len(length(snail_numbers)))
  drop <- which(grid$n1 == grid$n2)
  grid <- grid[-drop,]
  grid$mags <- NA
  for (i in seq_len(nrow(grid))) {
    this_pair <- snail_numbers[c(grid$n1[i], grid$n2[i])]
    grid$mag[i] <- calc_magnitude(add_snail_numbers(this_pair))
    if (verbose) if (i %% 100 == 0) cat(i, "\n")
  }
  return(max(grid$mag))
}

stopifnot(calc_max_magnitude("day18/test_input.txt") == 3993)

tic("day 18, part 2")
stopifnot(calc_max_magnitude("day18/input.txt") == 4725) # part 2
toc(log = TRUE)

