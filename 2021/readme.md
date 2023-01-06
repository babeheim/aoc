
# Solvers and Solutions

- Day 02 using [{R6} objects](https://github.com/karawoo/adventofcode2021/blob/main/R/day02.R#L98-L150)
- Day 08 [animation](https://www.reddit.com/r/adventofcode/comments/rbuvq3/2021_day_8_part_2pygame_code_breaker/)
- Day 09 [also using igraph](https://twitter.com/rappa753/status/1468876602016735233)
- Day 10 [using regmatches](https://twitter.com/TeaStats/status/1469239054625648645)
- Day 14 [animation](https://twitter.com/nbardiuk/status/1470761538969645067)
- Day 18 https://twitter.com/TeaStats/status/1472276208519983112
- Day 19 https://twitter.com/ashbaldry_/status/1472566622535798785
- Day 19 using linear regression to infer rotation: https://twitter.com/mccorvie/status/1472684849123102720
- day 24 using an optimizer https://twitter.com/tipadaknife/status/1474450242842017799
- day 25 in R https://twitter.com/TeaStats/status/1475092918746689541



# [Day 25: Sea Cucumber](https://adventofcode.com/2021/day/25)

We have a grid of agents, some are "east-facing" and some are "south-facing", on a toroid. Each time step, the east-facing individuals each consider whether there's an open space in front of them and, if it's free, they all move. Then the south-facing individuals each consider whether there's an open space to the south and, if its free, they all move. It's a toroid, so those on the bottom of the map consider the top of the map as their next spot, and those on the far east consider the western-most spot their next spot. The goal is to find the first step on which no sea cucumbers move?

The movement of each agent is fairly simple, so I'll use a matrix shuffle.



# [Day 24: Arithmetic Logic Unit](https://adventofcode.com/2021/day/24)

```r

rm(list = ls())

inp <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- b
    b_arg <- paste0("in=", b)
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("writing", b_arg, "to register", a)
  return(reg_list)
}

add <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- reg_list[[a]] + b
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- reg_list[[a]] + reg_list[[b]]
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("adding", a_arg, "and", b_arg)
  return(reg_list)
}

mul <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- reg_list[[a]] * b
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- reg_list[[a]] * reg_list[[b]]
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("multiplying", a_arg, "and", b_arg)
  return(reg_list)
}

div <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  if (b == 0L) stop("cannot divide by zero!")
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- reg_list[[a]] %/% b
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- reg_list[[a]] %/% reg_list[[b]]
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("dividing", a_arg, "by", b_arg)
  return(reg_list)
}

mod <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  if (a < 0L) stop("cannot modulo a negative dividend!")
  if (b == 0L) stop("cannot modulo by zero!")
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- reg_list[[a]] %% b
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- reg_list[[a]] %% reg_list[[b]]
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("modulo", a_arg, "by", b_arg)
  return(reg_list)
}

eql <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- as.integer(reg_list[[a]] == b)
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- as.integer(reg_list[[a]] == reg_list[[b]])
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("testing equality between", a_arg, "and", b_arg)
  return(reg_list)
}


alu <- list(inp = inp, add = add, mul = mul, div = div, mod = mod, eql = eql)

run_line <- function(line, reg_list, input = NULL) { 
  l <- as.list(strsplit(line, " ")[[1]])
  if (l[[1]] == "inp") {
    if (!is.null(input)) {
      l[[3]] <- input
    } else {
      stop("inp input missing!")
    }
  }
  if (!(l[[3]] %in% c("w", "x", "y", "z"))) {
    l[[3]] <- as.integer(l[[3]])
    stopifnot(!is.na(l[[3]]))
  }
  out <- alu[[l[[1]]]](l[[2]], l[[3]], reg_list)
  return(out)
}

regs <- list(w = 0, x = 0, y = 0, z = 0)
eql("w", 3L, regs)
eql("w", "x", regs)

regs <- list(w = 0, x = 0, y = 0, z = 0)
regs <- inp("w", 5L, regs)
regs <- inp("x", 5L, regs)
regs <- mul("w", "x", regs)
regs$w == 25L

regs <- list(w = 0, x = 0, y = 0, z = 0)
regs <- inp("w", 5L, regs)
regs <- mul("w", -1L, regs)
regs$w == -5L

regs <- list(w = 0, x = 0, y = 0, z = 0)
regs <- run_line("inp w", regs, 15L)
regs <- run_line("add z w", regs)
regs <- run_line("mod z 2", regs)
regs <- run_line("div w 2", regs)
regs <- run_line("add y w", regs)
regs <- run_line("mod y 2", regs)
regs <- run_line("div w 2", regs)
regs <- run_line("add x w", regs)
regs <- run_line("mod x 2", regs)
regs <- run_line("div w 2", regs)
regs <- run_line("mod w 2", regs)

run_program <- function(path, inputs) {
  lines <- readLines(path)
  input_list <- vector("list", length(lines))
  inp_lines <- grep("inp", lines)
  stopifnot(length(inputs) == length(inp_lines))
  for (i in seq_along(inp_lines)) {
    input_list[[inp_lines[i]]] <- inputs[i]
  }
  regs <- list(w = 0, x = 0, y = 0, z = 0)
  log <- vector("list", length(lines))
  log[[1]] <- regs
  for (l in seq_along(lines)) {
    regs <- run_line(lines[l], regs, input_list[[l]])
    log[[l]] <- regs
  }
  log <- rbind(log)
  log <- dplyr::bind_rows(log)
  log <- dplyr::select(log, msg, w, x, y, z)
  log <- as.data.frame(log)
  return(log)
}

asm_program <- function(path, outpath) {
  lines <- readLines(path)
  inp_lines <- grep("inp", lines)
  inp_vars <- gsub("inp ", "", lines[inp_lines])
  inp_ins <- paste0("ins[", seq_along(inp_lines), "]")
  to_var_list <- strsplit(lines, " ")
  to_var <- rep(NA, length(lines))
  for (line in seq_along(to_var_list)) {
    to_var[line] <- to_var_list[[line]][2]
  }
  lines <- gsub("add ", "sum(", lines)
  lines <- gsub("mul ", "prod(", lines)
  lines <- gsub("mod ", "`%%`(", lines)
  lines <- gsub("div ", "`%/%`(", lines)
  lines <- gsub("eql ", "`==`(", lines)
  lines <- paste0(lines, ")")
  lines <- gsub(" ", ", ", lines)
  lines <- paste(to_var, "<-", lines)
  lines[inp_lines] <- paste(inp_vars, "<-", inp_ins)
  writeLines(lines, outpath)
}

# asm_program("day24/input.txt", "day24/input.R")

run_program("day24/binary_converter.txt", 1L)
# sweet, but i want a full report!

run_monad <- function(path, input) {
  # have to specify input as a 14-digit number, no zeros!'
  monad_input <- as.integer(strsplit(as.character(input), "")[[1]])
  stopifnot(all(monad_input %in% 1:9))
  stopifnot(length(monad_input) == 14L)
  out <- run_program(path, monad_input)
  return(out)
}

run_monadR <- function(path, input) {
  ins <- as.integer(strsplit(as.character(input), "")[[1]])
  w <- x <- y <- z <- 0
  source(path, local = TRUE)
  out <- list(w = w, x = x, y = y, z = z)
  return(out)
}

x <- run_monad("day24/input.txt", 11111111111111)$z[252]
run_monadR("day24/input.R", 11111111111111)$z == x
run_monadR("day24/input.R", 11111111111111)$z == x

run_monad("day24/input.txt", 12934998949199) # part 1
run_monadR("day24/input_refactor.R", 12934998949199)

# the biggest integer the machine can represent is 2147483647, only 10 digits!
for (i in 1:1000) {
  seed <- as.numeric(paste(sample(1:9, 14, replace = TRUE), collapse = ""))
  x <- run_monad("day24/input.txt", seed)$z[252]
  stopifnot(run_monadR("day24/input_refactor.R", seed)$z == x)
  if (i %% 100 == 0) print(i)
}

```

With the code refactored, we can finally see the structure of the MONAD program. We start with z = 0. Each digit of input serves as a logical gate. The first three gates, x1 to x3, MUST be 1. The fourth could be 1 or 0

```r

# 12934998949199 - the biggest number possible

input <- 11711691612189 # part 2 finished
ins <- as.integer(strsplit(as.character(input), "")[[1]])

z0 <- as.numeric(ins[1] + 13L) * 26L + (ins[2] + 10L)
# ins[1] sets the initial count for 26s (14:22)
# ins[2] controls initial remainder (11:19)
g1 <- as.integer(!(ins[3] - 6L == ins[4])) # g1=0 if ins[3] == ins[4] + 6
stopifnot(g1 == 0)
g2 <- as.integer(!(ins[5] + 5L == ins[6])) # g2=0 if ins[5] == ins[6] - 5
stopifnot(g2 == 0)

z6 <- z0 +
  g1 * z0 * 25L + g1 * (ins[4] + 14L) +
  g2 * z0 * 25L + g2 * (ins[6] + 15L) +
  g1 * g2 * (25L * 25L * z0 + 25L * (ins[4] + 14L))

z7 <- z6 * 26L + (ins[7] + 4L) # ins[7] controls remainder for z7, 5:13
z8 <- z7 * 26L + (ins[8] + 11L) # ins[8] controls remainder for z8, 12:20

g3 <- as.integer(!(ins[9] - 5L == ins[10])) # g3=0 if ins[9] == ins[10] + 5
stopifnot(g3 == 0)
z10 <- z8 + g3 * (z8 * 25L + (ins[10] + 15L))
x11 <- as.integer(!((z10 %% 26L) - 10L == ins[11])) # x11=0 if ins[8] + 1 == ins[11]
stopifnot(x11 == 0)
z11 <- (z10 %/% 26L) + x11 * ((z10 %/% 26L) * 25L + ins[11] + 12L) # (z10 %/% 26L) could be z8 or z7
x12 <- as.integer(!((z11 %% 26L) - 12L == ins[12])) # x12=0 if ins[7] - 8 == ins[12]
stopifnot(x12 == 0)
z12 <- (z11 %/% 26L) + x12 * ((z11 %/% 26L) * 25L + ins[12] + 8L)

x13 <- as.integer(!((z12 %% 26L) - 3L == ins[13])) # set ins[2], check vs ins[13]
stopifnot(x13 == 0)
z13 <- (z12 %/% 26L) + x13 * ((z12 %/% 26L) * 25L + ins[13] + 14L)
x14 <- as.integer(!((z13 %% 26L) - 5L == ins[14]))
stopifnot(x14 == 0)
z <- (z13 %/% 26L) + x14 * ((z13 %/% 26L) * 25L + ins[14] + 9L)
stopifnot(z == 0)

```



# [Day 23: Amphipod](https://adventofcode.com/2021/day/23)

A spatial puzzle! We have to instruct amphipods of four types of amphipod, A, B, C and D, into their corresponding room in a spatial map that looks like this:

```
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
```

Amphipods can only move into spaces that are empty and can move at most twice (once to the empty hallway, and once into their correct room. The cost of moving once space for an A amphipod is 1, for a B is 10, for a C is 100 and for a D is 1000, and we want to assort them using a minimal cost.



# [Day 22: Reactor Reboot](https://adventofcode.com/2021/day/22)



# [Day 21: Dirac Dice](https://adventofcode.com/2021/day/21)

Dirac Dice is a two-player game that consists of a single die, two pawns (one for each player), and a game board with a circular track and ten marked spaces or position, 1 to 10, clockwise. Each player's starting place is chosen, and they take turns moving. On each player's turn, the player rolls the die three times, adds up the results (a value which depends on how many sides the die has!), and moves their pawn that many times around the track. The value of the space they land on is the number of points they get, added to a score that starts at 0. In both parts, our task is to figure out which player will win the game, at first in a deterministic system, then a stochatic one.

In Part One, we use deterministic, 100-sided dice! That is, the dice always start at 1, then 2, then 3, up to 100 in sequence, starting over again at 1 after 100. The first player rolls {1, 2, 3} and so moves 6 spaces. The second player rolls {4, 5, 6} and so moves 15 spaces, etc. The first player to reach a score of 1000 wins

In principle the total score of a player on a particular turn can be calculated exactly if we know a player's starting position and how many moves they've experienced by the end of that turn. To do it, we just calculate the sum of these two values, i + k, score the number of complete loops and the remaining uncomplete loop, and subtract the score of a(i). For example, if they started on position 2, and moved 3 spaces, then their score is just a(5) - a(2) = 3 + 4 + 5. If they instead moved 17 spaces, that's `a(((2 + 17) - 1) %% 10 + 1) = a(9)` = 45 points for completing the initial loop, `a(10) * (((2 + 17) - 1) %/% 10)` = 55 points for the 1 complete loop minus a(2) = 3, or 52 points.

We just need to know the cumulative number of moves each player gets by the end of any a particular turn. Let's start with the number *on* a give turn - for our deterministic die, the number of moves on each turn $i$ is $3(3i - 1)$. The sum of all moves up to and including turn $i$ is $3i(3i+1)/2$, which can be decomposed into the sum of moves in all even turns, which go to player 2, and all odd terms, which go to player 1.

Ok, now we use a three-sided "Dirac die", and must explore *all possible outcomes of each roll of the die*. Here a game ends when a player's score reaches 21. Here I solved it by creating an array that indexes five properties to define a game state - the board positions and scores, and the turn number, and then counted timelines iteratively.



# [Day 20: Trench Map](https://adventofcode.com/2021/day/20)

We're given an "image enhancement algorithm" (a bit string of 512 characters) and an "input image" (a bit map). Each input image pixel and the eight adjacent pixels in the grid are combined by concatenation to produce an index for the bit string, and the pixel stored at that index is the output pixel for this map.

But what to do with edges? Ah, there's an infinite border of dark pixels around the image. It's like Conway's Game of Life!

Here's my proposal:
1. Initialize a "large enough" matrix size m x n for the task. How to define 'large enough'? As a general rule, it seems like you can anticipate for n enhancements, you'll need n layers of 0s around the starting matrix, which can be added as two rbinds and two cbinds.
2. Identify all cells that are adjacent to a '1' in any direction, so have to be updated. How do we know if a pixel is to be updated? which(x == 1) with some math maybe?
a. for all pixels i with a 1, add the values to the lazy list: c(c(i-1,i,i+1), c(i-1,i,i+1) - n, c(i-1,i,i+1) + n).
b. then just ask if 1:(n*m) is %in% that lazy list; those are the pixels to cycle through and update
3. For each cell j,k in the matrix, if that cell is to be updated, concatenate the j-1, j and j+1 rows of (k-1):(k+1) as a bit vector, convert to a decimal l, and replace the j,k value with `dict[l+1]`.

A key assumption I had was that only pixels with any neighboring 1s need to be updated...that works, provided the first dict entry is 0 right? ah, but it's not! the zeros are all being updated

ok, so clearly i dont understand how the margins are supposed to work ehre...something is really weird about this updating system

the best i can do is add a buffer to 2 times the number of enhancements, then remove one of the two buffer layers



# [Day 19: Beacon Scanner](https://adventofcode.com/2021/day/19)

Scanners can detect any nearby beacons *relative* to their position, where nearby means <= 1000 m in x,y,z space. Scanners cannot detect other scanners and we do not know the absolute position of each scanner. Further, the scanners do not know their relative orientations either, except that they are pointed along an axis (so 24 different possible orientations)

Dataset structure and notation: each of the five? scannners exist in a list of dataframes called `s`, with coordinate columns `x`, `y`, `z` for all beacons and [0, 0, 0] for the scanner itself (add this if not present). We will be comparing coordinates between pairs of dataframes, so one of them is "reference" and the other "comparison", and the coordinates of the other one will be translated into a new "candidate" x,y,z space (details in the algorithm), `xc`, `yc`, `zc`. When we have a match between our reference scanner and comparison scanner, we add the comparison scanner's the location relative to the reference scanner into the reference scanner dataframe, as well as any additional coordinates not present already in the reference dataframe.

Here's an algorithm to do this:
while there is more than one data frame in the list `s`....
1. pick a reference and a comparison data frame
2. pick the 24 most distant beacons in the comparison dataframe (the problem says you're guaranteed to find 12 overlapping in SOME reference-comparison pairs, but we don't know which 12, so might as well go bigger)
3. for each possible orientation, for each of the distant beacons in candidate, for each beacon in reference, calculate all locations in the comparison dataframe (including both the beacons and the comparison scanner itself) and store those as the candidate `cx`, `cy` and `cz`
4. if all candidate beacons that are within 1000m of the reference scanner are also present in the reference dataframe in exactly the same position, AND all reference beacons that are with 1000m of the candidate scanner are also present in the candidate dataframe in exactly the same positions, we have a *match*, so stop the for-loop early.
5. if a match is found, identify the coordinates that are not present in the reference dataframe but in the comparison dataframe and add those to the reference dataframe coordinates, bringing in `cx`, `cy` and `cz` (make sure this includes the comparison scanner as well, annotated as such). then remove the "reference" dataframe from the list

the while loop will repeat this algorithm until all coordinates have been absorbed into one data frame, at which point we've solved part 1

ok, here's a simpler algorithm to get the 24 orientations
begin with a key axis side, +x, as 'red'. rotate along the x axis 0-3 times to get the 4 orientations with red as +x. rotate red twice to get it as -x, and do the same 0-3 rotations along the x axis. now rotate red up to be +z. rotate along the z axis 0-3 times. then rotate red down to -z and rotate along the z axis 0-3 times. finally, rotate red to +y and rotate 0-3 along y axis. and rotate red -y and do the same. 6 * 4 = 24 unique orientations, vs 128 as I was doing it before...
so we need *one* or *three* rotations followed by 0:4 rotations along the A axis

(always starting from the same orientation)
0-3) rotate +90 * 0-3 times around the x axis (yz plane)
4-11) rotate c(+90, -90) on z axis (xy plane), and for each then rotate +90 * 0-3 times on the y axis (xz plane)
12-19) rotate c(+90, -90) on the y axis (xz plane), and for each, +90 * 0-3 times on the z axis (xy plane)
20-23) rotate 2 * +90 on y axis (xz plane) then +90 * 0-3 times on the x axis (yz plane)

there's also a *linear* seqeunce of transofrms that cycle through all 24 in a very simple manner, let's just do those...
imagine we are looking at an 'enterprise' like shape head on, with the x axis in front, the +y-axis to the ship's left, and the z axis to the ship's top, by the right-hand-rule. a ccw rotate of +90 on x is called (+x), of -90 is (-x), etc.
(+x)(+x)(+x) (+z)  (-y)(-y)(-y) (+x) (+z)(+z)(+z) (-y) (+x)(+x)(+x) (+z) (-y)(-y)(-y) (+x) (+z)(+z)(+z)
if i can specify each of these as an orientation to apply to the input dynamically it cycle through all 24 orientations!


```r

rm(list = ls())

library(dplyr)

apply_transform <- function(data, n_turn_z = 0, n_turn_y = 0, n_turn_x = 0) {
  theta_z <- n_turn_z * (pi/2)
  theta_y <- n_turn_y * (pi/2)
  theta_x <- n_turn_x * (pi/2)
  A_x <- round(matrix(
    c(
      1, 0, 0,
      0, cos(theta_x), -sin(theta_x),
      0, sin(theta_x), cos(theta_x)
    ), byrow = TRUE, ncol = 3))
  A_y <- round(matrix(
    c(
      cos(theta_y), 0, sin(theta_y),
                 0, 1, 0,
      -sin(theta_y), 0, cos(theta_y)
    ), byrow = TRUE, ncol = 3))
  A_z <- round(matrix(
    c(
      cos(theta_z), -sin(theta_z), 0,
      sin(theta_z), cos(theta_z), 0,
      0, 0, 1
    ), byrow = TRUE, ncol = 3))
  xyz_data <- as.matrix(data[,c("x", "y", "z")])
  # note: the order you apply these determines the outcome!
  xyz_data <- t(A_x %*% t(xyz_data))
  xyz_data <- t(A_y %*% t(xyz_data))
  xyz_data <- t(A_z %*% t(xyz_data))
  data$x <- xyz_data[,1]
  data$y <- xyz_data[,2]
  data$z <- xyz_data[,3]
  return(data)
}

test_e <- list(
  # bridge
  list(x = 2, y = 0, z = 0.85),
  # saucer
  list(x = 2.5, y = 0, z = 0.75),
  list(x = 2.354, y = 0.354, z = 0.75),
  list(x = 2, y = 0.5, z = 0.75),
  list(x = 1.646, y = 0.354, z = 0.75),
  list(x = 1.5, y = 0, z = 0.75),
  list(x = 1.646, y = -0.354, z = 0.75),
  list(x = 2, y = -0.5, z = 0.75),
  list(x = 2.354, y = -0.354, z = 0.75),
  # neck
  list(x = 1.125, y = 0, z = 0.1875),
  list(x = 1.250, y = 0, z = 0.375),
  list(x = 1.375, y = 0, z = 0.5625),
  # engineering
  list(x = 0.75, y = 0, z = 0),
  list(x = 0.5, y = 0, z = 0),
  list(x = 0.25, y = 0, z = 0),
  list(x = 0, y = 0, z = 0),
  list(x = -0.25, y = 0, z = 0),
  # port nacelle pylon
  list(x = 0, y = 0.2, z = 0.1),
  list(x = 0, y = 0.4, z = 0.2),
  list(x = 0, y = 0.6, z = 0.3),
  list(x = 0, y = 0.8, z = 0.4),
  list(x = 0, y = 1, z = 0.5),
  # port nacelle
  list(x = -1.5, y = 1, z = 0.5),
  list(x = -1, y = 1, z = 0.5),
  list(x = -0.5, y = 1, z = 0.5),
  list(x = 0.5, y = 1, z = 0.5),
  # starboard nacelle pylon
  list(x = 0, y = -0.2, z = 0.1),
  list(x = 0, y = -0.4, z = 0.2),
  list(x = 0, y = -0.6, z = 0.3),
  list(x = 0, y = -0.8, z = 0.4),
  list(x = 0, y = -1, z = 0.5),
  # starboard nacelle
  list(x = -1.5, y = -1, z = 0.5),
  list(x = -1, y = -1, z = 0.5),
  list(x = -0.5, y = -1, z = 0.5),
  list(x = 0.5, y = -1, z = 0.5)
) %>% bind_rows() %>% as.data.frame()

library(plot3D)
d2 <- apply_transform(test_e, n_turn_x = 3)
points3D(d2$x, d2$y, d2$z, xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2), ticktype = "detailed", bty = "g", col = "black", pch = 20, phi = 0, theta = 90) # phi = 0, theta = 90

# (+x)(+x)(+x) (+z)  (-y)(-y)(-y) (+x) (+z)(+z)(+z) (-y) (+x)(+x)(+x) (+z) (-y)(-y)(-y) (+x) (+z)(+z)(+z)



match_scanners <- function(s, verbose = FALSE) {
  orientations <- expand.grid(n_turn_z = 0:3, n_turn_y = 0:3, n_turn_x = 0:3)
  while(length(s) > 1) {
    if (verbose) print(paste(length(s) - 1, "scanners remain unmatched"))
    picks <- sample(1:length(s), 2)
    ref <- s[[picks[1]]]
    for (i in 1:nrow(orientations)) {  # for each orientation i
      com <- s[[picks[2]]]
      com <- apply_transform(com, n_turn_z = orientations$n_turn_z[i],
        n_turn_y = orientations$n_turn_y[i],
        n_turn_x = orientations$n_turn_x[i]
      )
      for (j in 1:nrow(com)) { # for each j in the comparison set
        # calculate the coordinates in com with point j as new origin
        com$x_j <- com$x - com$x[j]
        com$y_j <- com$y - com$y[j]
        com$z_j <- com$z - com$z[j]
        stopifnot(com$x_j[j] == 0)
        stopifnot(com$y_j[j] == 0)
        stopifnot(com$z_j[j] == 0)
        for (k in 1:nrow(ref)) {
          # calculate in com assuming com point j (@ which com$x_j[j] = 0) is ref point k
          com$x_jk <- com$x_j + ref$x[k]
          com$y_jk <- com$y_j + ref$y[k]
          com$z_jk <- com$z_j + ref$z[k]
          stopifnot(com$x_jk[j] == ref$x[k])
          stopifnot(com$y_jk[j] == ref$y[k])
          stopifnot(com$z_jk[j] == ref$z[k])
          # having done the translation, see if the points match up
          within_ref <- integer()
          for (l in 1:sum(!ref$is_beacon)) {
            within_ref <- c(within_ref, 
              which(
                com$is_beacon &
                abs(com$x_jk - ref$x[which(!ref$is_beacon)[l]]) <= 1000 &
                abs(com$y_jk - ref$y[which(!ref$is_beacon)[l]]) <= 1000 &
                abs(com$z_jk - ref$z[which(!ref$is_beacon)[l]]) <= 1000
              )
            )
          }
          com_matches <- all(
            com$x_jk[within_ref] %in% ref$x &
            com$y_jk[within_ref] %in% ref$y &
            com$z_jk[within_ref] %in% ref$z
          )
          if (length(within_ref) >= 12 & com_matches) {
            if (verbose) print(paste("all reference points matched connecting ref point", k, "and com point", j))
            add <- com[-within_ref,]
            add$x <- add$x_jk
            add$y <- add$y_jk
            add$z <- add$z_jk
            add <- dplyr::select(add, x, y, z, is_beacon, scanner)
            s[[picks[1]]] <- dplyr::bind_rows(s[[picks[1]]], add)
            # having matched the comparison scanner, remove that dataframe from s
            s <- s[-picks[2]]
            break()
          }
        } # for each point k in reference set
        if (com_matches) break()
      } # for each point j in the comparison set
      if (com_matches) break()
    } # for each orientation i
  } # while some scanners unmatched
  return(s[[1]])
}

raw <- readLines("day19/input.txt")
scanner_lines <- grep("scanner", raw)

n_scanners <- length(scanner_lines)
s <- vector("list", n_scanners)

for (i in 1:n_scanners) {
  if (i < n_scanners) {
    add_raw <- raw[(scanner_lines[i] + 1):(scanner_lines[i + 1] - 1)]
  } else {
    add_raw <- raw[(scanner_lines[i] + 1):length(raw)]
  }
  drop <- which(add_raw == "")
  if (length(drop) > 0) add_raw <- add_raw[-drop]
  add <- data.frame(
    x = rep(NA, length(add_raw)),
    y = rep(NA, length(add_raw)),
    z = rep(NA, length(add_raw))
  )
  for (j in 1:length(add_raw)) {
    add[j,] <- as.numeric(strsplit(add_raw[j], ",")[[1]])
  }
  add$is_beacon <- TRUE
  add$scanner <- NA
  add <- dplyr::bind_rows(add, list(x = 0, y = 0, z = 0, is_beacon = FALSE, scanner = (i - 1)))
  s[[i]] <- add
}

out <- match_scanners(s)

stopifnot(sum(out$is_beacon) == 79) # test data

stopifnot(sum(out$is_beacon) == 483) # real data

scanners <- out[which(!out$is_beacon),]
n_scanners <- nrow(scanners)

diff_x <- matrix(NA, nrow = n_scanners, ncol = n_scanners)
diff_y <- matrix(NA, nrow = n_scanners, ncol = n_scanners)
diff_z <- matrix(NA, nrow = n_scanners, ncol = n_scanners)
for (k in 1:nrow(diff_x)) {
  for (j in 1:nrow(diff_x)) {
    diff_x[j, k] <- abs(scanners$x[j] - scanners$x[k])
    diff_y[j, k] <- abs(scanners$y[j] - scanners$y[k])
    diff_z[j, k] <- abs(scanners$z[j] - scanners$z[k])
  }
}

manhatten <- diff_x + diff_y + diff_z

stopifnot(max(manhatten) == 14804)

```



# [Day 18: Snailfish](https://adventofcode.com/2021/day/18)




# [Day 17: Trick Shot](https://adventofcode.com/2021/day/17)

Here we're tasked with calculating a ballistic trajectory in two dimensions, trying to hit a rectangular target area. At each time step, our probe's x-position updates by its x-velocity at the previous timestep, so $x_{t+1} = x_t + v_t$, and its' y-position updates the same. The probe begins at coordinates 0,0 and its velocity in both dimensions decreases by one each timestep from the initial values. The x-velocity slows to 0 and stops (due to drag), but the y-velocity continues to decrement by 1 without end (due to gravity). The probe has successfully reached the target if its coordinates are within the target boundaries at the end of a discrete time step - speeding through the target to the other side doesn't count.

In Part One, we calculate maximum possible vertical height the probe can attain and still make it to the target zone. In Part Two, we calculate all possible paths that make it to the target zone, at least for velocity values in the natural numbers.

We can solve Part One without a computer. A key fact is that the target is *below* the origin, so the probe must complete a full parabola, return to the starting depth, and then continue on to the target depth. If the probe left with velocity $v_y$, it must have a velocity of -(v_y + 1) upon returning to the starting location. If this velocity is *greater* than $-d$, the lower boundary of the target rectangle, the probe will already be past the target upon finishing the next time step. So, the fastest the probe can go is $-d - 1$, which also attains the maximum height. For any velocity $v_y$, the maximum height of the parabola is $v_0 (v_0 +1)/2$, so the maximum to still hit the target is just $-d (-d + 1) / 2$.

I solved Part Two by brute force, exploring every combination of velocities within a range to see which ones hit the target. However, we can use more math to set smart boundaries, limiting our exploration just to trajectories that are reasonable. We already saw that $v_y$ cannot be faster than $-d$, so we don't need to consider any velocity greater than that. Similarly, we don't need to consider velocities less than d, since if fired down, the probe will over-shoot the target otherwise. We can limit our evaluation to the range d to (-d - 1), inclusive. Similarly, our x-velocity range is limited. If we set $v_x$ to b, where b is the right-most wall of the rectangle, we will reach it in one time step. Conversely, if we set $v_x$ to the smallest value that satisfies $v_x (v_x + 1)/2 \leq a$, where $a$ is the left boundary, we will just cross into the target zone before drag halts our movement. This boundary condition turns out to be $\sqrt(8a + 1) - 1)/2$. This defines the range on x!

At this point, it's easy to brute force, and we're done.

```r

test_shot <- function(v0, a, b, c, d) {
  v0 <- as.numeric(v0)
  i <- 0:(2*(abs(d) + 1) + 1)
  v_x <- v0[1] - i
  v_x[v_x < 0] <- 0
  v_y <- v0[2] - i
  x <- c(0, cumsum(v_x))
  y <- c(0, cumsum(v_y))
  any(a <= x & x <= b & d <= y & y <= c)
}

count_trajectories <- function(a, b, c, d) {
  v_x_range <- ceiling((sqrt(8 * a + 1) - 1)/2):b
  v_y_range <- d:(-(d+1))
  dat <- expand.grid(v_x_range, v_y_range)
  dat$hit <- apply(dat, 1, test_shot, a, b, c, d)
  sum(dat$hit)
}

stopifnot(count_trajectories(20, 30, -5, -10) == 112)
stopifnot(count_trajectories(269, 292, -44, -68) == 996)

```



# Day 16: Packet Decoder

https://adventofcode.com/2021/day/16

We are to decode nested packets from a hexadecimal bitstring. Each packet begins with three bits that encode the *packet version*. The next three bits encode the *packet type ID*. The next set of bits in the packet depends on the packet type ID, either holding a big-endian number called the *literal value* or holding additional packets nested within the packet. The outermost packet may also end with trailing zeros, which are to be ignored.

Packets with a packet type ID of 4 are the ones holding a literal value, a single binary number. The number is broken up into four-bit big-endian subsets called groups. Each group is preceeded by a 1-bit except the last group, which is preceeded by a 0-bit. The groups together are then padded with trailing zeros until they are a multiple of 4 bits.

The other packet type ID values indicate a specific operation that is to be performed on the sub-packets within the packet. They are:

- 0: sum of values in sub-packets
- 1: product of values in sub-packets
- 2: minimum of values in sub-packets
- 3: maximum of values in sub-packets
- 4: a literal value packet
- 5: greater than packets, returns 1 if value of first sub-packet is greater than value of second sub-packet, otherwise returns 0; must have exactly two sub-packets
- 6: less than packets, returns 1 if value of first sub-packet is less than value of second sub-packet, otherwise returns 0; must have exactly two sub-packets
- 7: equals to packets, returns 1 if value of first sub-packet is equal to than value of second sub-packet, otherwise returns 0; must have exactly two sub-packets

In Part One, we are asked to calculate the sum of the version number a packet and all sub-packets recursively. In Part Two we are to perform all the operations within a packet.



# Day 15: Chiton

https://adventofcode.com/2021/day/15

Given a two-dimensional array of integers, find the up-down-left-right path that has the lowest total value. You start in the top-left cell, and you don't count that cell's value in your total score, but every other cell along the path is added together to get the total risk score.



# [Day 14: Extended Polymerization](https://adventofcode.com/2021/day/14)

Here we want to know how many atoms will be involved in a polymer growth process. Start with a string like `NNCB` which represents four atoms in a chain, and apply insertion rules like `NC -> B`. This means, for a sequence of elements `N` and `C` we insert a `B` element in-between them. All insertions happen simultaneously, lengthening the polymer chain. The starting input is a chain of 20 atoms. Our target is to count up the most common and least common elements in the resulting polymer after 10 growth steps (in Part One) and afte 40 steps (in Part Two).

Initially I tried just growing the new vector, but things quickly get intractable. The vector doubles every step, so for an initial length of $x$, after $n$ steps the vector is length $2^n (x-1) + 1$. The initial 10 steps is no problem, as the vector is now only 19,457 atoms long. However, after 15 or so steps we are at ~600,000 elements and my machine locks up, no where close to 40 steps, when the full polymer will have around 21 *trillion* elements! Clearly we need another strategy.

The key is the problem only wants *counts* of each element, not the actual polymer sequence itself. We can book-keep those in a matrix, as each pair of atoms produces "two" pairs of atoms recursively. As long as we remember that each atom is being counted twice (except for the outer two atoms in the initial polymer), we just recurse the count vector.




# [Day 13: Transparent Origami](https://adventofcode.com/2021/day/13)

Here, we have to "fold" a set of coordinates along specific axes, hopefully decoding a message.



# [Day 12: Passage Pathing](https://adventofcode.com/2021/day/12)

Part One:

> Your goal is to find the number of distinct paths that start at start, end at end, and don't visit small caves more than once. There are two types of caves: big caves (written in uppercase, like A) and small caves (written in lowercase, like b). It would be a waste of time to visit any small cave more than once, but big caves are large enough that it might be worth visiting them multiple times. So, all paths you find should visit small caves at most once, and can visit big caves any number of times.

Part Two:

> After reviewing the available paths, you realize you might have time to visit a single small cave twice. Specifically, big caves can be visited any number of times, a single small cave can be visited at most twice, and the remaining small caves can be visited at most once. However, the caves named start and end can only be visited exactly once each: once you leave the start cave, you may not return to it, and once you reach the end cave, the path must end immediately.



# [Day 11: Dumbo Octopus](https://adventofcode.com/2021/day/11)

There are 100 octopuses arranged neatly in a 10 by 10 grid. Each octopus slowly gains energy over time and flashes brightly for a moment when its energy is full. 

![](day11/animation.gif)

Each time step, the energy levels increase by 1. An octopus with a level greater than 9 flashes, which increasing the energy levels of all adjacent octopuses by 1, including those diagonally adjacent. If this causes any neighbors to exceed an energy level of 9, they also flash. All octopuses who flashed then go to an energy of 0.

In Part One, we want to know how many flashes will occur after a certain number of steps. In Part Two, we want to know the first step when all octopuses will flash simultaneously.



# [Day 10: Syntax Scoring](https://adventofcode.com/2021/day/10)

Lines with correct syntax have nested opening and closing brackets that all match. A "corrupted" line opens on one left bracket (either `(`, `[`, `{`, or `<`) but closes on the wrong right bracket (either `)`, `]`, `}`, `>`). An "incomplete" line is not corrupted, but has un-matched right brackets.

In Part One, our task is to find the corrupted lines and score them. In Part Two, we discard the corrupted lines and find the incomplete lines, complete them, score the completions, and return the median score. I'll do everything in one function, `analyze_lines()`.



# [Day 9: Smoke Basin](https://adventofcode.com/2021/day/9)

Given a 2-dimensional matrix of digits, in Part One we are asked to find the local minimums, considering up-down and left-right differences only. In Part Two, we have to measure the size of the *basins* around each minimum, including all values except the 9s which represent the borders between basins, and multiply the sizes of the three largest basins. Initially I did an adjacency matrix approach using the {igraph} package, which works, but the simpler solution is just a `while` loop.

![](day09/basins.png)



# [Day 8: Seven Segment Search](https://adventofcode.com/2021/day/8)

The seven segments of a seven-segment display are labelled `a` thru `g`, and can display any digit from 0 to 9:

```
   0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

```


We are given ciphers in the following format:

```
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
fdgacbe cefdb cefbgd gcbe
```

Before the `|` separator, we have ten patterns which represent the ten digits, but the meaning of each letter has been scrambled with respect to the correct segment for that digit. After the `|` separator is a set of four digits encoded by this cipher, which we want to de-cipher. There are ten such ciphers in the training set, and two hundred ciphers in the validation set.



# [Day 7: The Treachery of Whales](https://adventofcode.com/2021/day/7)

In Part One, we want to find the (integer) value which minimizes the total absolute deviation of a list of integers representing fuel costs. In Part Two, we want to find the value to minimize the total *cumulative* absolute deviation (e.g. for a list entry 10 and a candidate value 5, there is an absolute deviation of 5 and a cumulative deviation of 1 + 2 + 3 + 4 + 5 = 15).

The value that minimizes total absolute deviation is just the [median](https://en.wikipedia.org/wiki/Median). The cumulative deviation of $n$ is given by $n(n + 1)/2$ and so is directly proportional to the squared deviation $n^2$, which is minimized by the [arithmetic mean](https://en.wikipedia.org/wiki/Arithmetic_mean). Because this isn't quite perfect, and because we must produce integers, the output has to check the local area around the true mean.

```r

stopifnot(total_cost_cumulative("day07/test_input.txt") == 168)
# "position 5 has total cumulative fuel cost 168"
plot(1:16, total_cost_cumulative("day07/test_input.txt", 1:16))
points(5, 168, pch = 20)

total_cost_cumulative("day07/input.txt") == 96987919
# "position 479 has total cumulative fuel cost 96987919"
total_cost_cumulative("day07/input.txt", 478)
# "position 478 has total cumulative fuel cost 96987874"
total_cost_cumulative("day07/input.txt", 477)
# "position 477 has total cumulative fuel cost 96988829"

x <- 450:500
plot(x, total_cost_cumulative("day07/input.txt", x))
points(479, 96987919, pch = 20)

# local grid search finds the answer of 96987874

```




# [Day 6: Lanternfish](https://adventofcode.com/2021/day/6)

Here we model a population of exponentially-growing, immortal lanternfish. Each mature fish can be categorized in one of seven states, representing the number of days remaining until reproduction, from 6 to 0. At the end of day 0, the fish reproduces, and begins the next day in state 6 again along with its offspring. Offspring fish take two days to mature and begin the reproductive cycle in state 6, so we could call those immature states 8 and 7. Given an initial population of fish with known states, we want to project the population size forward using these unconstrained growth rules.



# [Day 5: Hydrothermal Venture](https://adventofcode.com/2021/day/5)

We have the start- and stop-coordinates for lines of hydrothermal vents in discrete x-y space, in the format of

```
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
```

Our task is to count up the number of cells in which 2 or more such lines intersect as a sum. In Part One, we consider only horizonal and vertical lines. In Part Two, we consider diagonal lines as well.



# [Day 4: Giant Squid](https://adventofcode.com/2021/day/4)

For a given set of 5x5 bingo boards, find the board that will have the first "bingo" as you proceed through a given sequence of bingo draws. Also find the board that will have the *last* bingo through these draws, a guaranteed loser.



# [Day 3: Binary Diagnostic](https://adventofcode.com/2021/day/3)

Here we work with a list of binary numbers, all the same length. In Part One, we calculate two new numbers, `gamma` and `epsilon`. Each bit in `gamma` is the most-common bit in the corresponding position of the list. Each bit in `epsilon` is the least-common bit. For a given list, the product of gamma and epsilon is our target. In Part Two, we must find the unique number in the list (the "oxygen consumption score") that contains the most-common bit in the first position, in the second position, and so forth, and find the unique number in the list (the "CO2 scrubber score") that contains the least-common bit in the first position, in the second position, and so forth. Our target in Part Two is again the product of these two numbers.





# [Day 2: Dive!](https://adventofcode.com/2021/day/2)

We calculate our horizonal and vertical position after applying a list of text commands. In Part One, the command `forward 8` just means add 8 to our horizonal displacement, `up 8` means a decrease in depth by 8, and `down 2` an increase in depth of 2. In Part Two, we modify this interpretation: now `up 8` means decrease a quantity called *aim* by 8, and `down 2` means increase aim by 2. When we apply a `forward` command, we increase our horizonal displacement by the number given, and change our vertical displacement by the horizonal change times the aim.



# [Day 1: Sonar Sweep](https://adventofcode.com/2021/day/1)

For a list of measurements, how many are larger than the previous measurement in the list? For Part One, we are given the list of measurements. For Part Two, we construct the list from a longer list, as the rolling sum of every group of three measures.
