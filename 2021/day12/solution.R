
map_paths <- function(path, once = TRUE) {
  raw <- readLines(path)
  edges <- data.frame(
    node1 = substr(raw, 1, regexpr("-", raw)-1),
    node2 = substr(raw, regexpr("-", raw)+1, nchar(raw))
  )
  rooms <- sort(unique(c(edges[,1], edges[,2])))
  small_rooms <- rooms[!rooms %in% c("start", "end") & substr(rooms, 1, 1) %in% letters]

  paths <- list()
  backtracked_from <- vector("list", length = 50)
  path <- c("start")
  depth <- 1
  valid_paths <- c(edges[which(edges$node1 == "start"),]$node2, edges[which(edges$node2 == "start"),]$node1)
  while (path[depth] != "start" | length(valid_paths) > 0) {
    if (path[depth] != "end" & length(valid_paths) > 0) {
      # keep going
      path <- c(path, valid_paths[1])
      depth <- depth + 1
      small_already_visited <- path[which(path %in% small_rooms)]
      valid_paths <- unique(c(
        edges[which(edges$node1 == path[depth]),]$node2,
        edges[which(edges$node2 == path[depth]),]$node1
      ))
      # never return to the start
      valid_paths <- setdiff(valid_paths, "start")
      if (once | any(duplicated(small_already_visited))) {
        # filter rule: don't return to small rooms already visited
        valid_paths <- setdiff(valid_paths, small_already_visited)
      }
    } else {
      if (path[depth] == "end") paths <- c(paths, list(path))
      # backtrack and choose a different room
      backtracked_from[[depth]] <- character()
      backtracked_from[[depth - 1]] <- c(backtracked_from[[depth - 1]], path[depth])
      path <- path[-depth]
      depth <- depth - 1
      small_already_visited <- path[which(path %in% small_rooms)]
      valid_paths <- unique(c(
        edges[which(edges$node1 == path[depth]),]$node2,
        edges[which(edges$node2 == path[depth]),]$node1
      ))
      # never return to the start
      valid_paths <- setdiff(valid_paths, "start")
      if (once | any(duplicated(small_already_visited))) {
        # filter rule: don't return to small rooms already visited
        valid_paths <- setdiff(valid_paths, small_already_visited)
      }
      # since we are backtracking, also don't revisit the room you just came from
      valid_paths <- setdiff(valid_paths, backtracked_from[[depth]])
    }
  }
  return(paths)
}

stopifnot(length(map_paths("day12/test_input.txt")) == 10)
tic("day 12, part 1")
stopifnot(length(map_paths("day12/input.txt")) == 4167)
toc(log = TRUE)

stopifnot(length(map_paths("day12/test_input.txt", once = FALSE)) == 36)
tic("day 12, part 2")
stopifnot(length(map_paths("day12/input.txt", once = FALSE)) == 98441) # 216 seconds
toc()
