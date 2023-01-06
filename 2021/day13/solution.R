
fold_points <- function(path, n_folds = NULL) {
  raw <- readLines(path)
  folds <- raw[grep("^fold along", raw)]
  raw_coords <- raw[grep("^\\d", raw)]
  coords <- matrix(NA, ncol = 2, nrow = length(raw_coords))
  for (i in 1:length(raw_coords)) coords[i, ] <- as.numeric(strsplit(raw_coords[i], ",")[[1]])
  coords[,2] <- coords[,2] * (-1)
  if (!is.null(n_folds)) folds <- folds[1:n_folds]
  for (i in 1:length(folds)) {
    fold <- as.numeric(substr(folds[i], regexpr("=", folds[i]) + 1, nchar(folds[i])))
    if (grepl("y=", folds[i])) {
      fold <- fold * (-1)
      tar <- which(coords[,2] < fold)
      coords[tar,2] <- (2 * fold) - coords[tar,2]
    } else if (grepl("x=", folds[i])) {
      tar <- which(coords[,1] > fold)
      coords[tar,1] <- (2 * fold) - coords[tar,1]
    }
  }
  coords <- coords[-which(duplicated(coords)),]
  return(coords)
}

stopifnot(nrow(fold_points("day13/test_input.txt", n_folds = 1)) == 17)

stopifnot(nrow(fold_points("day13/input.txt", n_folds = 1)) == 689) # part 1

# to read part 2:
# plot(fold_points("day13/input.txt", n_folds = 689))
