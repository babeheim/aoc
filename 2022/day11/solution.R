
library(tictoc)
library(dplyr)

##############

update_worry <- function(worry, monkey, ops) {
  ops[[monkey]](worry)
}

test_item <- function(worry, monkey, mod = test_modulos) {
  worry %% mod[monkey] == 0
}

find_next_monkey <- function(current, test, links) {
  links$next_monkey[which(links$current_monkey == current & links$test_condition == test)]
}

# identify next_monkey for each item this round

simulate_transfers <- function(path, n_rounds = 20, use_modulo = TRUE) {

  x <- readLines(path)

  n_monkeys <- sum(grepl("Monkey \\d:", x))

  # create items table
  item_rows <- grep("  Starting items:", x)
  items <- data.frame(worry = numeric(), current_monkey = numeric())
  for (i in seq_along(item_rows)) {
    worry_scores <- gsub(".*: ", "", x[item_rows[i]])
    worry_scores <- as.numeric(strsplit(worry_scores, ", ")[[1]])
    add <- data.frame(worry = worry_scores, current_monkey = i)
    items <- bind_rows(items, add)
  }
  items$next_monkey <- NA

  # create links table
  links <- data.frame(current_monkey = numeric(), next_monkey = numeric(), test_condition = logical())
  link_rows <- grep("    If true: throw to monkey", x)
  for (i in seq_along(link_rows)) {
    add <- data.frame(
      current_monkey = i,
      # dont forget we index monkeys from 1
      next_monkey = as.numeric(gsub("\\D", "", x[link_rows[i]])) + 1,
      test_condition = TRUE
    )
    links <- bind_rows(links, add)
  }
  link_rows <- grep("    If false: throw to monkey", x)
  for (i in seq_along(link_rows)) {
    add <- data.frame(
      current_monkey = i,
      # dont forget we index monkeys from 1
      next_monkey = as.numeric(gsub("\\D", "", x[link_rows[i]])) + 1,
      test_condition = FALSE
    )
    links <- bind_rows(links, add)
  }
  links <- arrange(links, links$current_monkey)

  # extract test modulos
  mod_rows <- grep("  Test: divisible by ", x)
  test_modulos <- as.numeric(gsub("\\D", "", x[mod_rows]))

  supermodulo <- prod(test_modulos)

  # create operations list

  op_rows <- grep("  Operation:", x)
  if (use_modulo) {
    op_code <- paste0(gsub("  Operation: new = ", "function(old) floor((", x[op_rows]), ") / 3)")
  } else {
    op_code <- paste0(gsub("  Operation: new = ", "function(old) floor(", x[op_rows]), ")")
  }
  inspection_ops <- vector("list", length(op_code))
  for (i in seq_along(op_code)) inspection_ops[[i]] <- eval(parse(text = op_code[i]))


  # begin transfer simulation!

  counts <- data.frame(
    round = integer(),
    n_items = integer()
  )

  # loop over monkeys ("turns") then items within each monkey
  for (round in seq_len(n_rounds)) {
    for (m in seq_len(n_monkeys)) {
      my_items <- which(items$current_monkey == m)
      if (length(my_items) > 0) {
        for (i in my_items) {
          if (use_modulo) {
            items$worry[i] <- update_worry(items$worry[i], m, inspection_ops)
          } else {
            items$worry[i] <- update_worry(items$worry[i] %% supermodulo, m, inspection_ops)
          }
          items$test[i] <- test_item(items$worry[i], m, test_modulos)
          items$next_monkey[i] <- find_next_monkey(m, items$test[i], links)
        }
        # items immediately go to the next monkey
        items$current_monkey[my_items] <- items$next_monkey[my_items]
        items$next_monkey[my_items] <- NA
      }
      add <- data.frame(
        round = round,
        monkey = m,
        n_items = length(my_items)
      )
      counts <- bind_rows(counts, add)
    }
    # if (round %% 100 == 0) print(round)
  }

  counts |>
    group_by(monkey) |>
    summarize(
      total_items = sum(n_items)
    ) |> arrange(desc(total_items)) -> totals

  out <- as.numeric(totals$total_items[1]) * as.numeric(totals$total_items[2])

  return(out)

}


stopifnot(simulate_transfers("day11/test_input.txt", n_rounds = 20) == 10605)
tic("Day 11, part 1")
stopifnot(simulate_transfers("day11/input.txt", n_rounds = 20) == 72884)
toc(log=TRUE)

stopifnot(simulate_transfers("day11/test_input.txt", n_rounds = 10000, use_modulo = FALSE) == 2713310158)

tic("Day 11, part 2")
stopifnot(simulate_transfers("day11/input.txt", n_rounds = 10000, use_modulo = FALSE) == 15310845153)
toc(log=TRUE)
