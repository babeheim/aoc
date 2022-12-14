
# [Day 9: Encoding Error](https://adventofcode.com/2020/day/9)






# [Day 8: Handheld Halting](https://adventofcode.com/2020/day/8)

`acc` - increases or decreases the accumulator by the argument value (it initializes at 0), then executes next instruction
`jmp` - jumps to next instruction relative to itself
`nop` - no operation, executes the next instruction

```r

rm(list = ls())

acc <- function(state, value) {
  out <- state
  out$accumulator <- out$accumulator + value
  out$position <- out$position + 1
  return(out)
}

nop <- function(state, value) {
  return(acc(state, 0))
}

jmp <- function(state, value) {
  out <- state
  out$position <- out$position + value
  return(out)
}

run_instruction <- function(state, fun, value, ops = ops) {
  if (fun == "nop") out <- nop(state, value)
  if (fun == "jmp") out <- jmp(state, value)
  if (fun == "acc") out <- acc(state, value)
  return(out)
}

swap_ops <- function(line) {
  if (grepl("^jmp", line)) {
    out <- gsub("jmp", "nop", line)
  } else {
    out <- gsub("nop", "jmp", line)
  }
  return(out)
}

find_halt <- function(path, debug = FALSE) {
  lines <- readLines(path)
  
  if (debug == FALSE) {
    # initialize state
    state <- list(
      accumulator = 0,
      position = 1
    )
    # initialize instruction counts
    counts <- rep(0, length(lines))

    while (!any(counts > 1)) {
      line <- lines[state$position]
      line_fun <- substr(line, 1, 3)
      line_value <- as.numeric(substr(line, 5, nchar(line)))
      state <- run_instruction(state, line_fun, line_value)
      counts[state$position] <- counts[state$position] + 1
      cat("running line", state$position, "\n")
    }

  } else {
    changes <- grep("^jmp|^nop", lines)
    lines_og <- lines
    for (i in seq_along(changes)) {
      lines <- lines_og
      lines[changes[i]] <- swap_ops(lines[changes[i]])

      # initialize state
      state <- list(
        accumulator = 0,
        position = 1
      )
      # initialize instruction counts
      counts <- rep(0, length(lines))

      while (!any(counts > 1) & state$position <= length(lines)) {
        line <- lines[state$position]
        line_fun <- substr(line, 1, 3)
        line_value <- as.numeric(substr(line, 5, nchar(line)))
        state <- run_instruction(state, line_fun, line_value)
        counts[state$position] <- counts[state$position] + 1
        cat("running line", state$position, "\n")
      }
      if (state$position > length(lines)) break()
    }
  }

  out <- state$accumulator
  if (state$position > length(lines)) cat("program terminated!\n")
  return(out)

}

stopifnot(find_halt("day08/test_input.txt") == 5)
stopifnot(find_halt("day08/input.txt") == 2025)


stopifnot(find_halt("day08/test_input.txt", debug = TRUE) == 8)
stopifnot(find_halt("day08/input.txt", debug = TRUE) == 2001)



```




# [Day 7: Handy Haversacks](https://adventofcode.com/2020/day/7)

how many colors can eventually contain one shiny gold bag?

```r

path <- "day07/test_input.txt"

x <- readLines(path)
search_pattern <- "^.*(?=\\sbags\\scontain)"
m <- gregexpr(search_pattern, x, perl = TRUE)
parents <- unlist(regmatches(x, m))

search_pattern <- "(?<=\\scontain\\s).*"
m <- gregexpr(search_pattern, x, perl = TRUE)
children <- unlist(regmatches(x, m))

children <- gsub("\\sbags", "", children)
children <- gsub("\\sbag", "", children)
children <- gsub("\\.$", "", children)
children <- strsplit(children, ",\\s")

for (i in 1:length(parents)) {
  for (j in 1:length(children[[i]])) {
    add <- data.frame(parent = parents[i], child = children[[i]][j])
    if (i == 1 & j == 1) {
      out <- add 
    } else {
      out <- dplyr::bind_rows(out, add)
    }
  }
}

# shiny gold to bright white
# shiny gold to bright white to light red
# shiny gold to bright white to dark orange
# shiny gold to muted yellow
# shiny gold to muted yellow to light red
# shiny gold to muted yellow to dark orange

# white, red, orange, yellow so four!
# how many distinct bag colors can (eventually) contain at least one shiny gold bag




# arbitrary number contained inside


parse_rules <- function(path) {
  x <- readLines(path)
  
}


```


# [Day 6: Custom Customs](https://adventofcode.com/2020/day/6)

```r

find_group_letters <- function(group, op = union) {
  xl <- strsplit(group, split = "")
  set <- xl[[1]]
  if (length(xl) > 1) {
    for (i in 2:length(xl)) {
      set <- op(set, xl[[i]])
    }
  }
  return(set)
}

count_letters <- function(path, op = union) {
  xs <- readLines(path)
  starts <- c(1, which(xs == "") + 1)
  stops <- c(which(xs == "") - 1, length(xs))
  out <- vector("list", length(starts))
  for (i in 1:length(out)) {
    out[[i]] <- find_group_letters(xs[starts[i]:stops[i]], op = op)
  }
  out <- unlist(lapply(out, length))
  out <- sum(out)
  return(out)
}

count_letters("day06/test_multigroup.txt", op = union) == 11
count_letters("day06/input.txt", op = union) == 6799


count_letters("day06/test_multigroup.txt", op = intersect) == 6
count_letters("day06/input.txt", op = intersect) == 3354




```



# [Day 5: Binary Boarding](https://adventofcode.com/2020/day/5)

For a given region, the next letter in the sequence (L or R, F or B) says which half of that region it is. 

The first seven letters specify a unique row of the 128. The last three letters specify a unique seat of 8 seats.

FBFBBFFRLR: row 44, column 5, seat ID 357.
BFFFBBFRRR: row 70, column 7, seat ID 567.
FFFBBBFRRR: row 14, column 7, seat ID 119.
BBFFBBFRLL: row 102, column 4, seat ID 820.

```r

calc_seat_id <- function(x) {
  if (length(x) == 1) {
    x <- strsplit(x, split = "")[[1]]
    h <- x[which(x %in% c("B", "F"))]
    h <- as.numeric(h == "B")
    row <- sum(h * 2^(length(h):1 - 1))
    w <- x[which(x %in% c("L", "R"))]
    w <- as.numeric(w == "R")
    col <- sum(w * 2^(length(w):1 - 1))
    out <- sum(8 * row + col)
  } else {
    out <- sapply(x, calc_seat_id)
    out <- as.numeric(out)
  }
  return(out)
}

calc_seat_id("FBFBBFFRLR") == 357
calc_seat_id("BFFFBBFRRR") == 567
calc_seat_id("FFFBBBFRRR") == 119
calc_seat_id("BBFFBBFRLL") == 820

find_highest_seat <- function(path) {
  x <- readLines(path)
  out <- max(calc_seat_id(x))
  return(out)
}


find_empty_seat <- function(path) {
  x <- readLines(path)
  x <- calc_seat_id(x)
  valid <- min(x):max(x)
  out <- valid[which(!(valid %in% x))]
  return(out)
}

find_highest_seat("day05/test_input.txt") == 820

find_highest_seat("day05/input.txt") == 930

find_empty_seat("day05/input.txt") == 515

```





# [Day 4: Passport Processing](https://adventofcode.com/2020/day/4)

```r

rm(list = ls())

library(dplyr)
library(yaml)

count_valid_passports <- function(path, strict = FALSE) {
  x <- readLines(path)
  starts <- c(1, which(x == "") + 1)
  stops <- c(which(x == "") - 1, length(x))
  passports <- rep(NA, sum(x == ""))
  for (i in seq_along(starts)) passports[i] <- paste(x[starts[i]:stops[i]], collapse = " ")

  for (i in 1:length(passports)) {
    add <- strsplit(passports[i], " ")[[1]]
    add <- gsub(":", ": '", add)
    add <- paste0(add, "'")
    add <- yaml.load(add)
    for (j in 1:length(add)) add[[j]] <- as.character(add[[j]])
    if (i == 1) {
      pass <- add
    } else {
      pass <- bind_rows(pass, add)
    }
  }

  if (!strict) {
    required_keys <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    # ignore cid
    pass$valid <- apply(pass[,required_keys], 1, function(z) !any(is.na(z)))
  } else {
    pass$bry_valid <- (
      nchar(pass$byr) == 4 & 
      pass$byr >= 1920 &
      pass$byr <= 2002
    )
    pass$iyr_valid <- (
      nchar(pass$iyr) == 4 & 
      pass$iyr >= 2010 &
      pass$iyr <= 2020
    )
    pass$eyr_valid <- (
      nchar(pass$eyr) == 4 & 
      pass$eyr >= 2020 &
      pass$eyr <= 2030
    )
    pass$hgt_val <- substr(pass$hgt, 1, regexpr("[a-z]", pass$hgt) - 1)
    pass$hgt_uni <- substr(pass$hgt, regexpr("[a-z]", pass$hgt), nchar(pass$hgt))
    pass$hgt_valid <- (
      pass$hgt_uni %in% c("in", "cm") &
      (
        (pass$hgt_uni == "in" & pass$hgt_val >= 59 & pass$hgt_val <= 76) |
        (pass$hgt_uni == "cm" & pass$hgt_val >= 150 & pass$hgt_val <= 193)
      )
    )
    pass$hcl_valid <- grepl("#[0-9a-f]{6}$", pass$hcl)
    pass$ecl_valid <- pass$ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    pass$pid_valid <- grepl("^\\d{9}$", pass$pid)
    valid_cols <- grepl("_valid", colnames(pass))
    pass$valid <- apply(pass[,valid_cols], 1, all)
  }

  out <- sum(pass$valid, na.rm = TRUE)
  return(out)
}

count_valid_passports("day04/test_input.txt") == 2
count_valid_passports("day04/input.txt") == 226


count_valid_passports("day04/test_input.txt", strict = TRUE) == 2
count_valid_passports("day04/input.txt", strict = TRUE) == 160

```


# [Day 3: Toboggan Trajectory](https://adventofcode.com/2020/day/3)

Starting at (1,1), we need to locate every entry that is x cells over and y cells down from the previous position.

```r

calc_tree_hits <- function(path, slope_x, slope_y) {
  stopifnot(length(slope_x) == length(slope_y))
  if (length(slope_x) == 1) {
    x <- readLines(path)
    x2 <- strsplit(paste(x, collapse = ""), "")[[1]]
    m <- matrix(as.numeric(x2 == "#"), ncol = nchar(x[1]), nrow = length(x), byrow = TRUE)

    n <- nrow(m) %/% slope_y - 1
    x <- (1 + slope_x * seq_len(n))
    # wrap-around the map
    x <- (x - 1) %% ncol(m) + 1
    y <- 1 + slope_y * seq_len(n)
    hits <- rep(NA, n)
    for (i in seq_len(n)) {
      hits[i] <- m[y[i],x[i]]
    }
    out <- sum(hits)
  } else {
    out <- rep(NA, length(slope_x))
    for (i in seq_along(slope_x)) {
      out[i] <- calc_tree_hits(path, slope_x[i], slope_y[i])
    }
    out <- prod(out)
  }
  return(out)
}

# part 1:
calc_tree_hits("day03/test_input.txt", 5, 1) == 3
calc_tree_hits("day03/input.txt", 3, 1) == 252

# part 2:
# slopes to check:
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.

calc_tree_hits("day03/test_input.txt", 1, 1) == 2
calc_tree_hits("day03/test_input.txt", 3, 1) == 7
calc_tree_hits("day03/test_input.txt", 7, 1) == 4
calc_tree_hits("day03/test_input.txt", 1, 2) == 2

slope_xs <- c(1, 3, 5, 7, 1)
slope_ys <- c(1, 1, 1, 1, 2)
calc_tree_hits("day03/test_input.txt", slope_xs, slope_ys) == 336

calc_tree_hits("day03/input.txt", slope_xs, slope_ys) == 2608962048


```


# [Day 2: Password Philosphy](https://adventofcode.com/2020/day/2)

We're given pairs of strings. The first string is the *policy* and the second string is the *password*. A policy of `1-3 a` means the password must contain `a` at least `1` and at most `3` times.

```r

sum_valid_passwords_part1 <- function(path) {
  x <- readLines(path)
  dat <- data.frame(
    policy = substr(x, 1, regexpr(":", x) - 1),
    password = substr(x, regexpr(":", x) + 2, nchar(x))
  )
  dat$character <- substr(dat$policy, regexpr("\\s", dat$policy) + 1, nchar(dat$policy))
  dat$min <- substr(dat$policy, 1, regexpr("-", dat$policy) - 1)
  dat$max <- substr(dat$policy, regexpr("-", dat$policy) +1, regexpr("\\s", dat$policy) - 1)
  dat$min <- as.numeric(dat$min)
  dat$max <- as.numeric(dat$max)
  dat$count <- NA
  for (i in seq_len(nrow(dat))) {
    dat$count[i] <- sum(strsplit(dat$password[i], "")[[1]] == dat$character[i])
  }
  dat$valid <- dat$count >= dat$min & dat$count <= dat$max
  out <- sum(dat$valid)
  return(out)
}

sum_valid_passwords_part1("day02/test_input.txt") == 2
sum_valid_passwords_part1("day02/input.txt") == 628


sum_valid_passwords_part2 <- function(path) {
  x <- readLines(path)
  dat <- data.frame(
    policy = substr(x, 1, regexpr(":", x) - 1),
    password = substr(x, regexpr(":", x) + 2, nchar(x))
  )
  dat$character <- substr(dat$policy, regexpr("\\s", dat$policy) + 1, nchar(dat$policy))
  dat$pos1 <- substr(dat$policy, 1, regexpr("-", dat$policy) - 1)
  dat$pos2 <- substr(dat$policy, regexpr("-", dat$policy) +1, regexpr("\\s", dat$policy) - 1)
  dat$pos1 <- as.numeric(dat$pos1)
  dat$pos2 <- as.numeric(dat$pos2)
  # exactly one of the two positions must be the character
  dat$pos1_matches <- (substr(dat$password, dat$pos1, dat$pos1) == dat$character)
  dat$pos2_matches <- (substr(dat$password, dat$pos2, dat$pos2) == dat$character)
  dat$count <- dat$pos1_matches + dat$pos2_matches
  stopifnot(all(dat$count %in% c(0, 1, 2)))
  dat$valid <- dat$count == 1
  out <- sum(dat$valid)
  return(out)
}

sum_valid_passwords_part2("day02/test_input.txt") == 1
sum_valid_passwords_part2("day02/input.txt") == 705

```



# [Day 1: Report Repair](https://adventofcode.com/2020/day/1)

For a list of numbers, find the pair that sum to 2020. The solution is the product of these two numbers.

We need something like an outer product, but for the sum instead. In R, the `outer` function defaults to product, e.g. `outer(x, x, "*")`, but can also use other operators! So, `outer(x, x, "+")` works for pairs. However, this is slightly inefficient because it computes each pair twice. And, in Part II, we have to find the outer product for a trio of numbers, so maybe just use loops? No! Use `expand.grid`.

```r

find_pair_sum <- function(path) {
  x <- as.integer(readLines(path))
  n <- length(x)
  pairs <- expand.grid(first = x, second = x)
  pairs$sum <- pairs$first + pairs$second
  hit <- which(pairs$sum == 2020L)[1]
  out <- pairs$first[hit] * pairs$second[hit]
  return(out)
}

find_pair_sum("day01/test_input.txt") == 514579L
find_pair_sum("day01/input.txt") == 445536L


find_trio_sum <- function(path) {
  x <- as.integer(readLines(path))
  n <- length(x)
  trios <- expand.grid(first = x, second = x, third = x)
  trios$sum <- trios$first + trios$second + trios$third
  hit <- which(trios$sum == 2020L)[1]
  out <- trios$first[hit] * trios$second[hit] * trios$third[hit]
  return(out)
}

find_trio_sum("day01/test_input.txt") == 241861950L
find_trio_sum("day01/input.txt") == 138688160L




```