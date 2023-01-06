
count_simple_numbers <- function(path) {
  x <- readLines(path)
  targets <- rep(NA, length(x))
  for (i in 1:length(x)) {
    targets[i] <- strsplit(x[i], " \\| ")[[1]][2]
  }
  # how many words in targets have 2, 3, 4 or 7 characters?
  targets <- strsplit(targets, split = "\\s")
  counts <- unlist(lapply(targets, function(z) sum(nchar(z) %in% c(2, 3, 4, 7))))
  sum(counts)
}

# easy game bap bap
stopifnot(count_simple_numbers("day08/test_input.txt") == 26)
tic("day 08, part 1")
stopifnot(count_simple_numbers("day08/input.txt") == 412)
toc(log = TRUE)

#####################

code_segments <- function(seg, key, direction = "encode") {
  # for each string in seg, substitute each letter for the corresponding letter in the cipher key
  if (length(seg) == 1) {
    out <- seg
    for (i in 1:nrow(key)) {
      if (direction == "encode") {
        out <- gsub(key$true[i], key$encoded[i], out)
      }
      if (direction == "decode") {
        out <- gsub(key$encoded[i], key$true[i], out)
      }
    }
  } else {
    out <- as.character(sapply(seg, code_segments, key, direction))
  }
  return(out)
}

## first solutoin begins here


id_digit <- function(seg) {
  # load the dictionary
  dict <- data.frame(
    digit = c(1, 7, 4, 5, 2, 3, 6, 0, 9, 8),
    segments = c(
      "CF",
      "ACF",
      "BCDF",
      "ABDFG", "ACDEG", "ACDFG",
      "ABDEFG", "ABCEFG", "ABCDFG",
      "ABCDEFG"
    )
  )
  # for each string in seg, find the corresponding digit in the dictionary
  if (length(seg) == 1) {
    seg_vec <- strsplit(seg, split = "")[[1]]
    check <- which(nchar(dict$segments) == nchar(seg))
    out <- numeric()
    if (length(check) == 0) stop("segment pattern wrong length, not in dictionary")
    for (i in 1:length(check)) {
      dict_vec <- strsplit(dict[check[i],]$segments, split = "")[[1]]
      if (all(seg_vec %in% dict_vec)) out <- dict[check[i],]$digit
    }
    if (length(out) == 0) stop("segment pattern not in dictionary")
  } else {
    out <- as.numeric(sapply(seg, id_digit))
  }
  return(out)
}

create_cipher <- function(seed) {
  set.seed(seed)
  out <- data.frame(
    true = LETTERS[1:7],
    encoded = sample(letters[1:7])
  )
  return(out)
}

# this could be more efficient...
infer_cipher <- function(ten) {
  # for a vector of ten segment patterns, infer the cipher using logical rules
  out <- data.frame(
    true = NA,
    encoded = letters[1:7]
  )

  tenl <- sapply(ten, strsplit, split = "")
  tend <- nchar(ten)

  # first, figure out what the `A` is comparing the two-character and three-character
  coded_A <- setdiff(tenl[[which(tend == 3)]], tenl[[which(tend == 2)]])
  out[out$encoded == coded_A,]$true <- "A"

  # next, identify which have to be either B or D
  coded_BD <- setdiff(tenl[[which(tend == 4)]], tenl[[which(tend == 2)]])

  # which letter appears once in the five-character strings but aren't in coded_BD?
  # that's E
  appear_once_fives <- names(which(table(unlist(tenl[which(tend == 5)])) == 1))
  coded_E <- setdiff(appear_once_fives, coded_BD)
  out[out$encoded == coded_E,]$true <- "E"
  # the one that is in coded_BD is B
  coded_B <- intersect(appear_once_fives, coded_BD)
  out[out$encoded == coded_B,]$true <- "B"
  # which solves D
  coded_D <- setdiff(coded_BD, coded_B)
  out[out$encoded == coded_D,]$true <- "D"

  # whatever appears three times and is not A or D must be G
  appear_thrice_fives <- names(which(table(unlist(tenl[which(tend == 5)])) == 3))
  coded_G <- setdiff(appear_thrice_fives, c(coded_A, coded_D))
  out[out$encoded == coded_G,]$true <- "G"

  # whatever appears twice in the sixes and is not coded_E
  appears_twice_sixes <- names(which(table(unlist(tenl[which(tend == 6)])) == 2))
  coded_C <- setdiff(appears_twice_sixes, c(coded_E, coded_D))
  out[out$encoded == coded_C,]$true <- "C"

  # whatever is left is F
  coded_F <- setdiff(tenl[[which(tend == 2)]], coded_C)
  out[out$encoded == coded_F,]$true <- "F"

  if (any(is.na(out$true))) stop("something is wrong!")
  out <- out[order(out$true),]
  return(out) 
}

decode_sum_targets <- function(path) {
  # the main function to solve day 8's work
  x <- readLines(path)
  patterns <- rep(NA, length(x))
  encoded_targets <- rep(NA, length(x))
  for (i in 1:length(x)) {
    patterns[i] <- strsplit(x[i], " \\| ")[[1]][1]
    encoded_targets[i] <- strsplit(x[i], " \\| ")[[1]][2]
  }
  patterns <- strsplit(patterns, split = "\\s")
  encoded_targets <- strsplit(encoded_targets, split = "\\s")
  decoded_targets <- vector("list", length(encoded_targets))
  decoded_digits <- rep(NA, length(encoded_targets))
  for (i in 1:length(patterns)) {
    this_dict <- infer_cipher(patterns[[i]])
    decoded_targets[[i]] <- code_segments(encoded_targets[[i]], this_dict, direction = "decode")
    decoded_digits[i] <- as.numeric(paste(id_digit(decoded_targets[[i]]), collapse = ""))
  }
  sum(decoded_digits)
}

stopifnot(decode_sum_targets("day08/test_input.txt") == 61229)
tic("day 08, part 2")
stopifnot(decode_sum_targets("day08/input.txt") == 978171)
toc(log = TRUE)
