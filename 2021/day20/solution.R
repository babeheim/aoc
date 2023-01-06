
bin_to_dec <- function(x) {
  sum((2^((length(x):1) - 1)) * x)
}

enhance_bitmap <- function(path, n_enhance, verbose = FALSE) {
  raw <- readLines(path)
  dict <- as.numeric(strsplit(raw[1], "")[[1]] == "#") # 512 entries
  d <- matrix(NA, nrow = (length(raw) - 3 + 1), ncol = nchar(raw[3]))
  for (j in 1:nrow(d)) d[j,] <- as.numeric(strsplit(raw[j + 2], "")[[1]] == "#")
  # pad out the bitmap
  buffer <- 2 * n_enhance
  margin_above <- matrix(0, nrow = buffer, ncol = ncol(d))
  d <- rbind(margin_above, d)
  d <- rbind(d, margin_above)
  margin_left <- matrix(0, nrow = nrow(d), ncol = buffer)
  d <- cbind(margin_left, d)
  d <- cbind(d, margin_left)
  n <- nrow(d)
  m <- ncol(d)
  if (n_enhance > 0) {
    for (enhance in 1:n_enhance) {
      # now update using the rules
      d_new <- d
      for (j in 1:n) {
        for (k in 1:m) {
          update_pixel <- 1 < j & j < n & 1 < k & k < m
          if (update_pixel) {
            bin_address <- c(d[(j-1),(k-1):(k+1)], d[j,(k-1):(k+1)], d[(j+1),(k-1):(k+1)])
            d_new[j,k] <- dict[bin_to_dec(bin_address) + 1]
          }
        }
      }
      d <- d_new
      if (verbose) print(paste(enhance, "enhance!"))
    }
  }
  # remove the buffer layers with pathological 'infinity effects'
  if (n_enhance > 0) {
    d <- d[-c(1:n_enhance, (n + 1) - 1:n_enhance),]
    d <- d[,-c(1:n_enhance, (n + 1) - 1:n_enhance)]
  }
  return(d)
}

# load the data

d0 <- enhance_bitmap("day20/test_input.txt", 0)
d1 <- enhance_bitmap("day20/test_input.txt", 1)

d2 <- enhance_bitmap("day20/test_input.txt", 2)
stopifnot(sum(d2) == 35) # test data

d50 <- enhance_bitmap("day20/test_input.txt", 50)
stopifnot(sum(d50) == 3351) # test data

par(mfrow = c(2, 2))
image(t(d0[nrow(d0):1,]), axes = FALSE)
image(t(d1[nrow(d1):1,]), axes = FALSE)
image(t(d2[nrow(d2):1,]), axes = FALSE)
image(t(d50[nrow(d50):1,]), axes = FALSE)




d0 <- enhance_bitmap("day20/input.txt", 0)
d1 <- enhance_bitmap("day20/input.txt", 1)

tic("day 20, part 1")
d2 <- enhance_bitmap("day20/input.txt", 2)
stopifnot(sum(d2) == 4873)
toc(log = TRUE)

tic("day 20, part 2")
d50 <- enhance_bitmap("day20/input.txt", 50)
sum(d50) == 16394
toc(log = TRUE)

# par(mfrow = c(2, 2))
# image(t(d0[nrow(d0):1,]))
# image(t(d1[nrow(d1):1,]))
# image(t(d2[nrow(d2):1,]))
# image(t(d50[nrow(d50):1,]))
