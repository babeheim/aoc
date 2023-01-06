
library(tictoc)

clean_drive <- function(path, part_one = TRUE) {
  x <- readLines(path)
  # smol modifications to create a real bash script
  x <- gsub("^dir", "mkdir", x)
  x[grep("^\\d", x)] <- paste("fallocate -l", x[grep("^\\d", x)])
  x <- gsub("\\$ ", "", x)
  x <- x[-which(x == "ls")]
  x[which(x == "cd /")] <- "cd _root"
  writeLines(x, "temp.sh")

  if (dir.exists("_root")) unlink("_root", recursive = TRUE)
  dir.create("_root")
  system("sh temp.sh")
  file.remove("temp.sh")

  files <- list.files("_root", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  dat <- file.info(files)
  unlink("_root", recursive = TRUE)
  dirs <- rownames(dat)[which(dat$isdir)]
  dat <- dplyr::filter(dat, !dat$isdir)
  dir_sizes <- rep(NA, length(dirs))
  for (i in seq_along(dirs)) {
    dir_sizes[i] <- sum(dat$size[grep(dirs[i], rownames(dat))])
  } 
  if (part_one == TRUE) {
    out <- sum(dir_sizes[dir_sizes < 100000])
  } else {
    total <- sum(dat$size)
    empty <- 70000000 - total
    to_delete <- 30000000 - empty
    out <- min(dir_sizes[dir_sizes > to_delete])
  }
  return(out)
}


stopifnot(clean_drive("day07/test_input.txt") == 95437)
tic("Day 07, part 1")
stopifnot(clean_drive("day07/input.txt") == 1449447)
toc(log = TRUE)

stopifnot(clean_drive("day07/test_input.txt", part_one = FALSE) == 24933642)
tic("Day 07, part 2")
stopifnot(clean_drive("day07/input.txt", part_one = FALSE) == 8679207)
toc(log=TRUE)
