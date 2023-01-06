



# [Day 25: Full of Hot Air](https://adventofcode.com/2022/day/25)

- https://www.reddit.com/r/adventofcode/comments/zwqd2f/2022_day_25_part_1_4_working_approaches_to_day_25/

Converted SNAFU inputs to decimals (aka denary numbers).

Added denary numbers.

Converted sum from denary to SNAFU using analytic expression.

Mathematically, a denary number A plus some baseline (1111...1)_5 equals a number B in base 5 whose digits are all two plus the number in base SNAFU. The number of digits in the baseline is just the order of magnitude of A in base-5, i.e. the ceiling of the base-5 logarithm of A.



# [Day 24: Blizzard Basin](https://adventofcode.com/2022/day/24)

Each time step you can only stay put or move into adjacent open tiles, but each time step, the location of the open tiles changes (meaning, you might have no where to go!). So it's a search through a maze, but the maze is changing each time step, and some routes might become failures.

The system is totally deterministic though, so we know what spots are open on what times; this is a multiverse system!

Because we have a finite set of possible spaces to move to, my strategy was to use a (many-worlds) BFS approach. Specifically, for any given space we might occupy in the current time step, we identify what spaces will be available in the next time step, and do so for every space that is reachable in the current time step. Then, we recursively iterate over those candidate spaces in the same fashion. In practice, we thus explore every possible next space until one of them is the desired destination, and we are done.

- online discussion recommends viewing the system in three dimensions (the third being time), then its just a 3D maze



# [Day 23: Unstable Diffusion](https://adventofcode.com/2022/day/23)

- [solution megathread](https://www.reddit.com/r/adventofcode/comments/zt6xz5/2022_day_23_solutions/)
- using a finite state machine approach? Maybe the [{datafsm}](https://jonathan-g.github.io/datafsm/) package
- [Great viz, coloration is time since last moement](https://www.reddit.com/r/adventofcode/comments/ztb52g/2022_day_23_part2_python_coloured_by_time_since/)

Movement rules for each elf:
- if no other elf is in any of the eight adjacent tiles, the focal elf does not move (they are antisocial)
- if another elf is nearby, the focal elf looks  sequentially in four directions and proposes movement if that tile, and the two adjacent tiles visible to the elf, are unoccupied (so, North if North, Northwest, and Northeast are all clear). They stop looking once they've made a proposal.
- After all proposals made, elves move IF they were the only ones to propose that spot (otherwise they stay put)

All elves have the same sequence of proposal directions each round, but between rounds, this sequence cycles down one.

My strategy is to keep track of the elves as a list of agents, with a row and column on a fixed grid, and a 'proposal' position. To make this work I am converting between the row-column notation and the linear index notation for a matrix. It's easy to identify conflicts via duplicated proposal positions.

But, as the cloud of elves expands, I encounter problems when they exceed the capacity of the matrix, so I am throwing a 'buffer' around the matrix. This is which is honestly unsatisfying, since I didn't know ahead of time whether the buffer was big enough, and I had to keep increasing it to prevent the simulation from crashing.

Another approach might be to track the x-y position not using a linear index or a matrix at all. This would have the advantage of being infinitely expandable.

[-] on day 23, rewrite the code so there is not 'map' matrix with rows and columns, and the elves store just x and y positions instead




# [Day 22: Monkey Map](https://adventofcode.com/2022/day/22)

Another spatial puzzle; given a starting place on a landscape, we follow instructions to move X steps or turn to the right or left 90 degrees. If we encounter obstacles we prematurely halt that specific movement, but otherwise just follow the instructions.

Part 1 treats the landscape as a torus, wrapping around each side. This is easy enough to program, but in part 2, we have a more complex topology - a cube map, with the six sides of the cube unfolded.

Any specific cube face is connected to four other cube faces, but in bespoke ways.

One way to describe the map is to create hard-wired boundaries. But I'd like to 

Ok, I think I got it. We need to be able to describe a cube of arbitrary size, then apply the input picture as a 'texture' on top of that cube.

The fundamental question of this problem is, how do you represent movement on the surface of a cube? My strategy: for side length n, an nxnx6 array. Each face has cardinal directions. When movement is *across* faces.

```R

rm(list = ls())

library(dplyr)


# what does a rotation do? when you provide a specific x and y on the face, the rotation causes the face to rotate! 

rotate_matrix_cw <- function(mat, n_times = 0) {
  n_times <- n_times %% 4
  if (n_times == 0) {
    out <- mat
  } else if (n_times == 1) {
    out <- t(mat)[,ncol(mat):1]
  } else if (n_times == 2) {
    # aka -2
    out <- t(mat)[,ncol(mat):1]
    out <- t(out)[,ncol(out):1]
  } else if (n_times == 3) {
    # aka -1
    out <- t(mat)[nrow(mat):1,]
  } else {
    stop("n_times invalid")
  }
  return(out)
}

rotate_position <- function(pos, n_side, rotation) {
  map <- matrix(1:(n_side^2), ncol = n_side)
  position_index <- map[pos$row, pos$col]
  map <- rotate_matrix_cw(map, n_times = rotation)
  new_position <- which(map == position_index, arr.ind = TRUE)
  out <- pos
  out$row <- as.integer(new_position[,"row"])
  out$col <- as.integer(new_position[,"col"])
  out$orientation <- as.integer((pos$orientation + rotation) %% 4)
  return(out)
}

move_once <- function(current_state, edges, terrain) {

  n_side <- dim(terrain)[1]

  if (current_state$orientation == 0L) {
    move <- list(row = 0L, col = 1L)
  } else if (current_state$orientation == 1L) {
    move <- list(row = 1L, col = 0L)
  } else if (current_state$orientation == 2L) {
    move <- list(row = 0L, col = -1L)
  } else if (current_state$orientation == 3L) {
    move <- list(row = -1L, col = 0L)
  } else {
    stop("orientation invalid")
  }

  next_state <- current_state
  next_state$row <- current_state$row + move$row
  next_state$col <- current_state$col + move$col

  if (next_state$col == 0L) {
    tar <- which(next_state$face == edges$from & edges$dir == "W")
    next_state$face <- edges$to[tar]
    next_state$col <- n_side
    next_state <- rotate_position(next_state, n_side, edges$rotation[tar])
  } else if (next_state$col == (n_side + 1L)) {
    tar <- which(next_state$face == edges$from & edges$dir == "E")
    next_state$face <- edges$to[tar]
    next_state$col <- 1L
    next_state <- rotate_position(next_state, n_side, edges$rotation[tar])
  } else if (next_state$row == 0L) {
    tar <- which(next_state$face == edges$from & edges$dir == "N")
    next_state$face <- edges$to[tar]
    next_state$row <- n_side
    next_state <- rotate_position(next_state, n_side, edges$rotation[tar])
  } else if (next_state$row == (n_side + 1L)) {
    tar <- which(next_state$face == edges$from & edges$dir == "S")
    next_state$face <- edges$to[tar]
    next_state$row <- 1L
    next_state <- rotate_position(next_state, n_side, edges$rotation[tar])
  }

  # having identified the proposed state, check whether it is blocked on the terrain map

  if (terrain[next_state$row, next_state$col, next_state$face] == 0L) {
    return(next_state)
  } else {
    return(current_state)
  }

}



# describing a cross-shaped cube net (not applicable to actual puzzle)
edges <- list(
  list(from = 1, to = 2, dir = "N", rotation = 0),
  list(from = 1, to = 4, dir = "S", rotation = 0),
  list(from = 1, to = 6, dir = "E", rotation = 0),
  list(from = 1, to = 5, dir = "W", rotation = 0),

  list(from = 2, to = 3, dir = "N", rotation = 0),
  list(from = 2, to = 1, dir = "S", rotation = 0),
  list(from = 2, to = 6, dir = "E", rotation = 1),
  list(from = 2, to = 5, dir = "W", rotation = -1),

  list(from = 3, to = 4, dir = "N", rotation = 0),
  list(from = 3, to = 2, dir = "S", rotation = 0),
  list(from = 3, to = 6, dir = "E", rotation = 2),
  list(from = 3, to = 5, dir = "W", rotation = 2),

  list(from = 4, to = 1, dir = "N", rotation = 0),
  list(from = 4, to = 3, dir = "S", rotation = 0),
  list(from = 4, to = 6, dir = "E", rotation = -1),
  list(from = 4, to = 5, dir = "W", rotation = 1),

  list(from = 5, to = 2, dir = "N", rotation = 1),
  list(from = 5, to = 4, dir = "S", rotation = -1),
  list(from = 5, to = 1, dir = "E", rotation = 0),
  list(from = 5, to = 3, dir = "W", rotation = 2),
  
  list(from = 6, to = 2, dir = "N", rotation = -1),
  list(from = 6, to = 4, dir = "S", rotation = 1),
  list(from = 6, to = 3, dir = "E", rotation = 2),
  list(from = 6, to = 1, dir = "W", rotation = 0)
) |> bind_rows() |> as.data.frame()

edges$rotation <- as.integer(edges$rotation)
edges$from <- as.integer(edges$from)
edges$to <- as.integer(edges$to)


# going west

n_side <- 10L
terrain <- array(0, dim = c(n_side, n_side, 6))
state <- list(row = 3L, col = 2L, face = 1L, orientation = 2L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 3L, col = 1L, face = 1L, orientation = 2L)))

state <- list(row = 4L, col = 2L, face = 1L, orientation = 2L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 4L, col = 1L, face = 1L, orientation = 2L)))

state <- list(row = 10L, col = 10L, face = 1L, orientation = 2L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 10L, col = 9L, face = 1L, orientation = 2L)))

state <- list(row = 1L, col = 2L, face = 1L, orientation = 2L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 1L, col = 1L, face = 1L, orientation = 2L)))

state <- list(row = 1L, col = 2L, face = 1L, orientation = 2L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 1L, col = 1L, face = 1L, orientation = 2L)))

state <- list(row = 1L, col = 2L, face = 2L, orientation = 2L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 1L, col = 1L, face = 2L, orientation = 2L)))

state <- list(row = 1L, col = 1L, face = 1L, orientation = 2L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 1L, col = n_side, face = 5L, orientation = 2L)))

state <- list(row = 10L, col = 1L, face = 1L, orientation = 2L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 10L, col = n_side, face = 5L, orientation = 2L)))

# going east

state <- list(row = 10L, col = 9L, face = 1L, orientation = 0L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 10L, col = 10L, face = 1L, orientation = 0L)))

state <- list(row = 10L, col = 10L, face = 1L, orientation = 0L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 10L, col = 1L, face = 6L, orientation = 0L)))

state <- list(row = 4L, col = 10L, face = 1L, orientation = 0L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 4L, col = 1L, face = 6L, orientation = 0L)))

# going north

state <- list(row = 2L, col = 1L, face = 1L, orientation = 3L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 1L, col = 1L, face = 1L, orientation = 3L)))

state <- list(row = 1L, col = 1L, face = 1L, orientation = 3L)
stopifnot(identical(move_once(state, edges, terrain), list(row = n_side, col = 1L, face = 2L, orientation = 3L)))

# going south

state <- list(row = n_side, col = n_side, face = 1L, orientation = 1L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 1L, col = n_side, face = 4L, orientation = 1L)))

state <- list(row = 9L, col = n_side, face = 1L, orientation = 1L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 10L, col = n_side, face = 1L, orientation = 1L)))

# harder problems....

state <- list(row = 1L, col = 3L, face = 5L, orientation = 3L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 3L, col = 1L, face = 2L, orientation = 0L)))

state <- list(row = 6L, col = n_side, face = 6L, orientation = 0L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 5L, col = n_side, face = 3L, orientation = 2L)))

state <- list(row = 1L, col = 3L, face = 6L, orientation = 3L)
stopifnot(identical(move_once(state, edges, terrain), list(row = 8L, col = n_side, face = 2L, orientation = 2L)))

```

Ok, except for the terrain mapping function, I've got everything I need to execute instructions!

```R

sim_movement2 <- function(path, verbose = FALSE) {

#  path <- "day22/input.txt"
  x <- readLines(path)

  # describing a cube net for part 2
  edges <- list(
    list(from = 1, to = 2, dir = "N", rotation = 2),
    list(from = 1, to = 4, dir = "S", rotation = 0),
    list(from = 1, to = 6, dir = "E", rotation = 1),
    list(from = 1, to = 3, dir = "W", rotation = -1),

    list(from = 2, to = 1, dir = "N", rotation = 2),
    list(from = 2, to = 5, dir = "S", rotation = 2),
    list(from = 2, to = 3, dir = "E", rotation = 0),
    list(from = 2, to = 6, dir = "W", rotation = 0),

    list(from = 3, to = 1, dir = "N", rotation = 1),
    list(from = 3, to = 5, dir = "S", rotation = -1),
    list(from = 3, to = 4, dir = "E", rotation = 0),
    list(from = 3, to = 2, dir = "W", rotation = 0),

    list(from = 4, to = 1, dir = "N", rotation = 0),
    list(from = 4, to = 5, dir = "S", rotation = 0),
    list(from = 4, to = 6, dir = "E", rotation = 0),
    list(from = 4, to = 3, dir = "W", rotation = 0),

    list(from = 5, to = 4, dir = "N", rotation = 0),
    list(from = 5, to = 2, dir = "S", rotation = 2),
    list(from = 5, to = 6, dir = "E", rotation = -1),
    list(from = 5, to = 3, dir = "W", rotation = 1),
    
    list(from = 6, to = 1, dir = "N", rotation = -1),
    list(from = 6, to = 5, dir = "S", rotation = 1),
    list(from = 6, to = 2, dir = "E", rotation = 0),
    list(from = 6, to = 4, dir = "W", rotation = 0)
  ) |> bind_rows() |> as.data.frame()

  if (path == "day22/test_input.txt") {
    brd_map <- list(
      list(face = 1, rows_before = 0, cols_before = 2, rotation = 0),
      list(face = 2, rows_before = 1, cols_before = 0, rotation = 0),
      list(face = 3, rows_before = 1, cols_before = 1, rotation = 0),
      list(face = 4, rows_before = 1, cols_before = 2, rotation = 0),
      list(face = 5, rows_before = 2, cols_before = 2, rotation = 0),
      list(face = 6, rows_before = 2, cols_before = 3, rotation = -1)
    ) |> bind_rows() |> as.data.frame()
  } else if (path == "day22/input.txt") {
    brd_map <- list(
      list(face = 1, rows_before = 0, cols_before = 1, rotation = 0),
      list(face = 2, rows_before = 3, cols_before = 0, rotation = 1),
      list(face = 3, rows_before = 2, cols_before = 0, rotation = 1),
      list(face = 4, rows_before = 1, cols_before = 1, rotation = 0),
      list(face = 5, rows_before = 2, cols_before = 1, rotation = 0),
      list(face = 6, rows_before = 0, cols_before = 2, rotation = 1)
    ) |> bind_rows() |> as.data.frame()
  } else {
    stop("invalid path")
  }

  edges$rotation <- as.integer(edges$rotation)
  edges$from <- as.integer(edges$from)
  edges$to <- as.integer(edges$to)

  # load board
  board_lines <- x[1:(which(x == "") - 1)]
  brd <- matrix(NA, nrow = length(board_lines), ncol = max(nchar(board_lines)))
  for (i in 1:length(board_lines)) {
    chars <- strsplit(board_lines[i], split = "")[[1]]
    for (j in 1:length(chars)) {
      if (chars[j] == ".") {
        brd[i, j] <- 0L
      } else if (chars[j] == "#") {
        brd[i, j] <- 1L
      }
    }
  }

  n_side <- min(dim(brd)) / 3 

  viz <- FALSE
  if (viz == TRUE) {
    par(bg = "black")
    plot(NULL, xlim = c(1, ncol(brd)), ylim = c(nrow(brd), 1))
    for (i in 1:nrow(brd)) {
      for (j in 1:ncol(brd)) {
        if (!is.na(brd[i,j])) {
          if (brd[i,j]) {
            cell_col <- "#00d9ff"
          } else {
            cell_col <- "#0051ff"
          }
          rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, col = cell_col, border = NA)
        }
      }
    }
    for (i in 1:nrow(brd_map)) {
      rect(brd_map$cols_before[i] * n_side + 1, brd_map$rows_before[i] * n_side + 1, brd_map$cols_before[i] * n_side + n_side, brd_map$rows_before[i] * n_side + n_side, col = "red")
      text(
        brd_map$cols_before[i] * n_side + (n_side + 1)/2,
        brd_map$rows_before[i] * n_side + (n_side + 1)/2,
        labels = i,
        srt = 90 * brd_map$rotation[i]
      )
    }
  }

  # map the real terrain from the brd object
 
  terrain <- array(NA, dim = c(n_side, n_side, 6))

  for (i in 1:6) {
    rs <- brd_map$rows_before[i] * n_side + 1
    cs <- brd_map$cols_before[i] * n_side + 1
    terrain[,,i] <- rotate_matrix_cw(brd[rs:(rs+(n_side-1)),cs:(cs+(n_side-1))], brd_map$rotation[i])
  }

  mv_string <- x[length(x)]
  mv_string <- gsub("\\s", "", mv_string)
  mv_string <- gsub("R", ",R,", mv_string)
  mv_string <- gsub("L", ",L,", mv_string)
  mv_string <- gsub(",+", ",", mv_string)
  instructions <- strsplit(mv_string, split = ",")[[1]]
  n_instructions <- length(instructions)

  # initial state: You begin the path in the leftmost open tile of the top row of tiles. Initially, you are facing to the right (from the perspective of how the map is drawn).
  state <- list(row = 1L, col = 1L, face = 1L, orientation = 0L)

  for (i in seq_len(n_instructions)) {
    if (instructions[i] == "L") {
      state$orientation <- as.integer((state$orientation - 1L) %% 4)
      if (verbose) cat("turned left\n")
    } else if (instructions[i] == "R") {
      state$orientation <- as.integer((state$orientation + 1L) %% 4)
      if (verbose) cat("turned right\n")
    } else {
      n_moves <- as.integer(instructions[i])
      for (j in seq_len(n_moves)) {
        last_row <- state$row
        last_col <- state$col
        last_face <- state$face
        state <- move_once(state, edges, terrain)
        if (state$row == last_row & state$col == last_col & state$face == last_face) {
          if (verbose) cat("moved", (j-1), "steps; hit barrier\n")
          break()
        }
        if (j == n_moves) {
          if (verbose) cat("moved", j, "steps\n")
        }
      }
    }
  }

  # translate everything back to the board coordinate system, including orientation!
  brd_state <- rotate_position(state, n_side, (-1) * brd_map$rotation[state$face])
  brd_row <- brd_map$rows_before[brd_state$face] * n_side + brd_state$row
  brd_col <- brd_map$cols_before[brd_state$face] * n_side + brd_state$col

  out <- brd_row * 1000 +
    brd_col * 4 +
    brd_state$orientation

  return(out)

}

stopifnot(sim_movement2("day22/test_input.txt") == 5031)
stopifnot(sim_movement2("day22/input.txt") == 145065)

```




# [Day 21: Monkey Math](https://adventofcode.com/2022/day/21)


root is doing an equality test, e.g. == not +
humn is now a parameter, not the value in the code. our goal is to figure out what goes in to produce an equality check


i solved by brute force extrapolation - look at the trend and see when the two input functions come closest together...iterative refinement and we got it
it would be nice if there was something more systematic, however...


```R

# scratch:
rm(list = ls())
eval(parse(text = "zed <- function() foo() + out()"))
eval(parse(text = "foo <- function() 1"))
eval(parse(text = "bar <- function() 2"))
eval(parse(text = "out <- function() foo() + bar()"))
zed()

```

People online refer to gradient descent, newton-raphson


# [Day 20: Grove Positioning System](https://adventofcode.com/2022/day/20)

The number moves forwards or backwards in the list the number of spots its value indicates, when it is its turn to go.



# [Day 19: Not Enough Minerals](https://adventofcode.com/2022/day/19)

look at this post: https://www.reddit.com/r/adventofcode/comments/zxkrl8/2022_day_16_part_1_so_its_time_for_me_to_learn/

robot factor takes 1 minute to make a robot, plus the necessary resrcs
each robot collects 1 unit of its resource type per minute

ore-collecting robots
clay-collecting robots
obsidian-collecting robots (uses clay)
geode-collecting robots (uses obsidian)

you start with one ore-collceting robot

all robots need ore

for a list of blueprints, figure out which one will maximize the number of geodes processed in 24 minutes 

so the tricky part is that the blueprints arent deterministic...you have multpile allocation plans with each blueprint! i dunno how you can program a simple game to do that...

This is a lot like Starcraft, which I have always wanted to understand in a mathematical sense...

quotes from the megathread of solutions:

" a bruteforce BFS,"

> The following two observations *are* provably correct, and enough for both parts of the problem:
> - if you already are producing more ore per minute than the ore cost of the most expensive robot, there is no benefit to purchasing additional ore robots. Likewise for clay and obsidian;
> - there is no benefit to waiting a turn to buy a robot if you could buy it immediately [bret: and its a robot you definitely SHOULD buy]. Therefore, if you choose not to buy anything, the next robot you buy must be one of the robots you *couldn't* already afford that turn [and a robot definitely need].
>   The above leads to a brute-force BFS whose states are (1) the amount of each ore you have, (2) the amount of each robot, (3) the current time, and (4) a bitmask of which robots you're allowed to purchase next.

more insights: https://www.reddit.com/r/adventofcode/comments/zpy5rm/2022_day_19_what_are_your_insights_and/

> Note that we can do a bit better: For any resource R that's not geode: if you already have X robots creating resource R, a current stock of Y for that resource, T minutes left, and no robot requires more than Z of resource R to build, and X * T+Y >= T * Z, then you never need to build another robot mining R anymore.

in particular: https://www.reddit.com/r/adventofcode/comments/zpy5rm/comment/j0v9g8t/?utm_source=share&utm_medium=web2x&context=3

> 4) Assume a utopistic scenario that you can produce a new Geode robot  (for free) every turn from now on starting at the current search state.  How much geode would that yield for you at the end? If it would yield  less than the absolute best amount of geode you have seen throughout the  search so far (this is why DFS search is good, it hits many terminal  nodes early), then you can cut the current search state off, as it can  never beat the current one. The effect of this was surprisingly good.

The key insights:
1. jump forwads to each decision point rather than step by step
2. prune using a simple rule, the theoretical upper limit vs your current running maximum
3. prune using the rule you shouldnt make more robots than the maximum needed for a given time period



# [Day 18: Boiling Boulders](https://adventofcode.com/2022/day/18)

We have an x-y-z coordinate for each voxel in a big blob. Our goal is to calculate the outside surface area (all sides that are not directly touching another voxel).

```R

rm(list = ls())

calc_surface_area <- function(path) {
  x <- readLines(path)
  dat <- matrix(NA, nrow = length(x), ncol = 3)
  for (i in 1:nrow(dat)) dat[i,] <- as.integer(strsplit(x[i], split = ",")[[1]])

  # reindex at 1
  dat[,1] <- dat[,1] - min(dat[,1]) + 1L
  dat[,2] <- dat[,2] - min(dat[,2]) + 1L
  dat[,3] <- dat[,3] - min(dat[,3]) + 1L

  # enclose array in a box of 0's
  n_x <- max(dat[,1]) + 2L
  n_y <- max(dat[,2]) + 2L
  n_z <- max(dat[,3]) + 2L
  dat[,1] <- dat[,1] + 1L
  dat[,2] <- dat[,2] + 1L
  dat[,3] <- dat[,3] + 1L

  ar <- array(0L, dim = c(n_x, n_y, n_z))

  for (i in 1:nrow(dat)) {
    ar[dat[i,1], dat[i,2], dat[i,3]] <- 1L
  }

  n_faces <- 0L

  for (i in 1:dim(ar)[1]) {
    for (j in 1:dim(ar)[2]) {
      n_faces <- n_faces + sum(diff(c(0, ar[i,j,], 0)) != 0)
    }
  }

  for (i in 1:dim(ar)[1]) {
    for (j in 1:dim(ar)[3]) {
      n_faces <- n_faces + sum(diff(c(0, ar[i,,j], 0)) != 0)
    }
  }

  for (i in 1:dim(ar)[2]) {
    for (j in 1:dim(ar)[3]) {
      n_faces <- n_faces + sum(diff(c(0, ar[,i,j], 0)) != 0)
    }
  }

  return(n_faces)

}

calc_surface_area("day18/test_input.txt") == 64
calc_surface_area("day18/test_input_ii.txt") == 10
calc_surface_area("day18/test_input_iii.txt") == 4332

calc_surface_area("day18/input.txt") == 4364


```

This seems relatively straightforwards. Going along a specific column or row or whatever, we simply have to ask whether we transition from empty to occupied +1, occupied to empty +1. Empty to empty is +0 and full full is +0. 

Part 2 is harder, because we are asked to count only the OUTSIDE surfaces. My approach in part 1 wont work at all, so we need to use a tree search algorithm!

How about a 'minesweeper' approac? for each air cell, we ask how many lava cells are nearby and store those in an array. it's easy enough to incorporate part 2, smply by asking whether the air cell can be reached by a tree search from the outside



# [Day 17: Pyroclastic Flow](https://adventofcode.com/2022/day/17)

It's like tetris!

-
+
J
I
o

how tall will the twoer get after 2022 rocks fall?

- chamber is exactly 7 units wide
- rock appears so left edge is 2 units from left wall, bottom edge is 3 units above highest rock in the room (or the floor)
- jet, fall, jet, fall, etc.
- the instant it touches a rock below it stops moving and the next rock appears and takes the next move in the move sequence

strategy:
- define a CONTROL POINT as the location of the bottom-left edge of the shape. this is within the piece except for the + piece
- we track the control point for each object in the simulation
- and once it is settled we move to the next object

i am not describngn the sequence of events perfectly...

1. push
2. fall
3. push
4. fall 
5. push
6. fall
7. push
8. comes to a rest

1. psuh
2. fall
3. push
4. fall
5. push
6. fall
7. push
8. comes to a rest

so fall first, then do a comes-to-a-rest check
if not, apply the push and loop


```R

rm(list = ls())

# sim_tetris("day17/test_input.txt", n_pieces = 1000000)
222 * (1000000000000 / 1000000) / 3600 / 24 / 365 # 7 years!

# We have to record the heights and pieces and figure out the pattern....


record_tetris <- function(path, n_pieces = 2022) {

  out <- rep(NA, n_pieces)

  jet_sequence <- strsplit(readLines(path), split = "")[[1]]
  jet_sequence <- ifelse(jet_sequence == ">", 1L, -1L)

  jet_seqlen <- length(jet_sequence)

  piece_list <- c("—", "+", "J", "I", "O")

  # inital parameter values
  arena_floor <- 0L # initalize this at 0 for scrolling
  current_floor <- 1L # start at level 1 for the floor
  jet_pointer <- 1L
  pieces_dropped <- 0L

  # initialize the arena
  ar <- matrix(0L, ncol = 7, nrow = 100)
  ar[current_floor,] <- 1L

  while (pieces_dropped < n_pieces) {

    current_piece <- piece_list[pieces_dropped %% 5L + 1L]

    # define the new spawn point
    sp_y <- current_floor + 4L - arena_floor # 3 empty lines separate
    sp_x <- 3L

    # spawn a piece
    piece_mat <- spawn_piece(current_piece)
    # locate in the arena
    piece_mat[, 1] <- piece_mat[, 1] + sp_x
    piece_mat[, 2] <- piece_mat[, 2] + sp_y

    falling <- TRUE
    hit_detected <- FALSE

    while (falling) {

      stopifnot(piece_mat[,1] <= 7L)
      stopifnot(piece_mat[,1] >= 1L)

      # apply the next push in the jet_sequence
      candidate_piece_mat <- piece_mat
      candidate_piece_mat[,1] <- candidate_piece_mat[,1] + jet_sequence[jet_pointer]

      hit <- check_collision(candidate_piece_mat, ar)
      if (!any(hit)) piece_mat <- candidate_piece_mat
      jet_pointer <- jet_pointer + 1L
      if (jet_pointer == (jet_seqlen + 1L)) jet_pointer <- 1L

      # vertical collision check
      candidate_piece_mat <- piece_mat
      candidate_piece_mat[,2] <- candidate_piece_mat[,2] - 1L
      hit <- check_collision(candidate_piece_mat, ar)
      if (!any(hit)) {
        piece_mat <- candidate_piece_mat
      } else {
        falling <- FALSE
        for (i in 1:nrow(piece_mat)) {
          ar[piece_mat[i,2], piece_mat[i,1]] <- 1L
        }
        current_floor <- max(which(apply(ar == 1, 1, any))) + arena_floor
        out[pieces_dropped + 1] <- current_floor
      }

    }

    pieces_dropped <- pieces_dropped + 1L
    if (pieces_dropped %% 1000 == 0) cat(pieces_dropped, "\n")
    
    if ((nrow(ar) - (current_floor - arena_floor)) < 10) {
#     cat("approaching end of arena\n")
      ar <- rbind(ar[51:nrow(ar),], matrix(0L, nrow = 50, ncol = 7))
      arena_floor <- arena_floor + 50L
    }

  }
 
  return(out)

}

levels <- record_tetris("day17/input.txt", n_pieces = 100000)
pieces <- 1:length(levels)

levels <- c(1, levels)
pieces <- c(0, pieces)

fit <- lm(levels ~ pieces)
res <- resid(fit)
plot(levels, res, type = "l")
# got it!

# it cycles according to the length of the sequences
# ~ 2626

# try to find the optimal autocorrelation lag in the residuals...

lags <- seq(1, 5000, by = 1)
rho <- rep(NA, length(lags))
i <- 15000
for (j in 1:length(lags)) rho[j] <- cor(res[i + 1:1000], res[i + 1:1000 - lags[j]])
lags[which(rho > 0.999)]
# bingo! perfect cycle is 1715

(res[20000 + 1:1715] - res[20000 + 1 * 1715 + 1:1715])[1]
# every cylce adds a small change: 0.003876718

1000000000000 %% 1715 
# 583090379 cycles, residual 15

# the first 100 or so moves dont really follow a pattern but then it starts following a regular cycle that loops every 1715 pieces
plot(levels[1 * 1715 + 1:1715] - levels[0 * 1715 + 1:1715])
abline(h = 0, lty = 2)

# a mathematical model of the cycling behavior:
pred_level <- function(x) {
  n_cycles <- (x %/% 1715)
  rem <- x %% 1715
  levels[1 * 1715 + rem] + 2616 * (n_cycles - 1)
}
# exact recovery!!
all(pred_level(100:100000) == levels[100:100000])

options(scipen = 999)
# 1525364431486 (too low!)
# 1525364431487 <- just right!
# 1525364431488 (too high!)

x <- resid(lm(levels ~ pieces))

i <- sample(1:length(x), 1)
plot(x - x[i], type = "l", xlim = c(1500, 2000))
abline(h = 0, lty = 2)


```

visualization! https://www.youtube.com/watch?v=Rwrp-Br97Ko&feature=emb_imp_woyt

with current code, it takes about 1 second to do 100 pieces
to simulate 1 trillion pieces, then, it would take about ten billion seconds, or 317 years!
obviously, there has to be a way to speed things up...

The reddit discussion seems to say: there's a deterministic pattern, you can use modulo to figure out what it is.
https://www.reddit.com/r/adventofcode/comments/zq0ob1/2022_day_17_part_2_help_finding_the_phase_of/





# [Day 16: Proboscidea Volcanium](https://adventofcode.com/2022/day/16)





# [Day 15: Beacon Exclusion Zone](https://adventofcode.com/2022/day/15)


# [Day 14: Regolith Reservoir](https://adventofcode.com/2022/day/14)


Sand pours in grain-by-grain from point 500,0

sand movement rules:
- falls down one step if possible
- if tile below is blocked, falls diagonally down and to the left
- if that tile is blocked, falls diagonally down and to the right
- if all three possible dests are blocked, sand comes to a rest and no longer moves

how many units of sand come to a rest, before sand falls into the abyss below? Part 2 seems to reference the idea there is no abyss at all....

It seems like we need an efficient way to track a large matrix of values, e.g. 1000 x 500.

> "I realized that both part 1 and part 2 can be solved by memoizing the path of each previous grain of sand and then starting at the last position before it landed rather than at (500,0). This worked quite nicely and sped up my solution about 20x."



# [Day 13: Distress Signal](https://adventofcode.com/2022/day/13)

Rules:
- if both values are integers, compare the integers. 
STOP IF the lower integer is on the left
ERROR IF the lower integer is on the right
- if both values are lists, compare the values inside each list.
STOP IF the left list is shorter than the right list
ERROR IF the right list is shorter than the left list
- if exactly one value is an integer, convert to a list containing that integer

Part II is tough - we have to use the 'compare' function to determine if any two are out of order and, if they are, reverse them. But what sorting algorithm can be used for this? Bubble sort?

What does R use? In `sort` there is a `method` argument which has four options:
- `quick`, Singleton (1969)'s implementation of Hoare's Quicksort method 
- `radix`, no ref
- `shell`, Shellsort (an O(n^{4/3}) variant from Sedgewick (1986)
- `auto`, which selects `radix` for short (less than 2^31 elements) numeric vectors, otherwise `shell`

Honestly I just need something I can code which is simple.



# [Day 12: Hill Climbing Algorithm](https://adventofcode.com/2022/day/12)

a is lowest, b then up to z
you can only go 1 letter up in elevation each step
"the elevation of the dest square can be much lower than the elevation of your current square" implies that you can go more than 1 letter down tho...

There's an S (the start) with elevantion a and an E (the goal) which has elevation z.

The goal is to do it in as few steps as possible. Sounds like Djikstra! The cost of movement determines which adjacent cells are available.

In part 2, we have to find the shortest path across all the shortest paths from *every* elevation a. Reddit gave a great hint: just create new links between each of of the elevation 'a' tiles and the start with 0 distance, so they all effectively serve as logical starting places.



# [Day 11: Monkey in the Middle](https://adventofcode.com/2022/day/11)

We have to represent a graph that things move through.
A directed edge list, with the rule itself!
A monkey list, with the operations each does.
There's always an additional operation after the monkey's operation, the worry level is divided by 3 and rounded down to the nearest integer.
A *turn* is all the items in one monkey's possession, but the item goes to the next monkey it will also get inspected by that monkey.
A *round* is all the monkeys going for one turn.
We also have an *item* list. The items have a *current value* and a *current monkey*. Each time step, we apply (1) the transformation described by that monkey, (2) identify the *next monkey* based on the edge list, and then we replace the current monkey with the next monkey.

This could be a dirac dice situation? We shall see!

We count the total number of times a monkey inspects an item over 20 rounds, and find the two most active monkeys.

Explanation of the supermodulo trick: https://www.reddit.com/r/adventofcode/comments/zih7gf/2022_day_11_part_2_what_does_it_mean_find_another/



# [Day 10: Cathode-Ray Tube](https://adventofcode.com/2022/day/10)

great viz: https://www.reddit.com/r/adventofcode/comments/zhmsg2/2022_day_10_sprites_and_not_the_elf_kind/

X starts at 1
`addx V` takes two cycles to complete; after two cycles, add `V` to the X register
`noop` takes one cycle to complete and has no effect



# [Day 9: Rope Bridge](https://adventofcode.com/2022/day/9)

[Reddit Solution Megathread](https://www.reddit.com/r/adventofcode/comments/zgnice/2022_day_9_solutions/)

https://twitter.com/kjmimmack/status/1601398760239943681

https://github.com/norbertkehrer/aoc-2022

it looks like this uses [Chebyshev distance](https://en.wikipedia.org/wiki/Chebyshev_distance) aka "chessboard distance". Contrast with Manhatten distance.

look at this amazing viz: https://www.reddit.com/r/adventofcode/comments/zgq3nr/2022_day_9_rope_pull/



# [Day 8: Treetop Tree House](https://adventofcode.com/2022/day/8)

[Reddit Solutions](https://www.reddit.com/r/adventofcode/comments/zfpnka/2022_day_8_solutions/)




# [Day 7: No Space Left On Device](https://adventofcode.com/2022/day/7)

[Reddit Solutions](https://www.reddit.com/r/adventofcode/comments/zesk40/2022_day_7_solutions/)


# [Day 6: Tuning Trouble](https://adventofcode.com/2022/day/6)

[Reddit Solutions](https://www.reddit.com/r/adventofcode/comments/zdw0u6/2022_day_6_solutions/)


# [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5)



# [Day 4: Camp Cleanup](https://adventofcode.com/2022/day/4)



# [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3)




# [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2)

Each round you play a game. Your score is the shape you selected, 1 = Rock, 2 for paper, 3 for scissors, plus the outcome: 0 for a loss, 3 for a draw, and 6 for a win.

The strategy guide gives each opponent's (pre-determined) choice and the recommended strategy to sneakily cheat. The opponent's choices are coded as:

A = Rock
B = Paper
C = Scissors

In the first version of the problem, the meaning of the response column is which choice you should pick:

X = Rock
Y = Paper
Z = Scissors

Together the opponent's letter and this letter give the score for that round, e.g. A X is a tie.

In the second version of the problem, the meaning of the response column is the desired outcome for that round:

X = lose
Y = draw
Z = win

Together with the first letter, this implies the choice of move that you should make, e.g. A X means you pick scissors.




# [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1)
