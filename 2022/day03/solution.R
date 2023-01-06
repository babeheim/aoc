
library(tictoc)

inventory_rucksack <- function(pack_string) {
  data.frame(
    part = c(rep(1, nchar(pack_string)/2), rep(2, nchar(pack_string)/2)),
    item = strsplit(pack_string, "")[[1]]
  ) |>
  group_by(part) |>
  count(item) |> as.data.frame() |>
  mutate(priority = match(item, c(letters, LETTERS))) -> inv
  inv$in_both_parts <- inv$item %in% intersect(inv$item[inv$part == 1], inv$item[inv$part == 2])
  return(inv)
}

calc_rucksack_priorities <- function(path, scenario = "misplaced") {
  packs <- data.frame(
    contents = readLines(path),
    priority = NA
  )
  if (scenario == "misplaced") {
    for (i in seq_len(nrow(packs))) {
      inv <- inventory_rucksack(packs$contents[i])
      packs$priority[i] <- inv$priority[which(inv$in_both_parts)][1]
    }
    out <- sum(packs$priority)
  } else if (scenario == "grouping") {
    n_groups <- nrow(packs) %/% 3
    packs$group <- rep(seq_len(n_groups), each = 3)
    for (i in seq_len(n_groups)) {
      for (j in 1:3) {
        add <- inventory_rucksack(packs$contents[which(packs$group == i)[j]])
        add$member <- j
        if (j == 1) {
          inv <- add
        } else {
          inv <- bind_rows(inv, add)
        }
      }
      inv$in_all_members <- inv$item %in% intersect(intersect(inv$item[which(inv$member == 1)], inv$item[which(inv$member == 2)]), inv$item[which(inv$member == 3)])
      packs$priority[which(packs$group == i)] <- inv$priority[which(inv$in_all_members)][1]
    }
    elf_groups <- packs |> group_by(group) |> summarize(priority = first(priority)) 
    out <- sum(elf_groups$priority)
  }
  return(out)
}


stopifnot(calc_rucksack_priorities("day03/test_input.txt") == 157)

tic("Day 03, part 1")
stopifnot(calc_rucksack_priorities("day03/input.txt") == 7597)
toc(log=TRUE)

stopifnot(calc_rucksack_priorities("day03/test_input.txt", scenario = "grouping") == 70)

tic("Day 03, part 2")
stopifnot(calc_rucksack_priorities("day03/input.txt", scenario = "grouping") == 2607)
toc(log=TRUE)
