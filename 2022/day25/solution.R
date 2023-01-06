
library(tictoc)

glyphs <- data.frame(
  sna = c("=", "-", "0", "1", "2"),
  dig = c(-2, -1, 0, 1, 2)
)

sna_to_dec <- function(x) {
  n_digits <- nchar(x)
  sym <- strsplit(x, split = "")[[1]]
  coefs <- glyphs$dig[match(sym, glyphs$sna)]
  out <- sum(coefs * 5^(n_digits:1 - 1))
  return(out)
}

dec_to_sna <- function(x) {
  n <- ceiling(log(x, 5))
  x <- x + 2 * sum(5^(0:n))
  vec <- numeric()
  val <- x
  while(n >= 0) {
    rem <- val %/% 5^n
    val <- val - rem * 5^n
    vec <- c(vec, rem)
    n <- n - 1
  }
  while (vec[1] == 0 & length(vec) > 1) vec <- vec[-1]
  vec <- vec - 2
  while (vec[1] == 0 & length(vec) > 1) vec <- vec[-1]
  out <- paste(glyphs$sna[match(vec, glyphs$dig)], collapse = "")
  return(out)
}

calc_fuel_input <- function(path) {
  x <- readLines(path)
  dec_to_sna(sum(sapply(x, sna_to_dec)))
}

tic("Day 25, part 1")
stopifnot(calc_fuel_input("day25/test_input.txt") == "2=-1=0")
toc(log=TRUE)

