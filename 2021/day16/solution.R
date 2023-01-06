
halfbit_to_bin <- function(x) {
  if (x == "0") out <- c(0, 0, 0, 0)
  if (x == "1") out <- c(0, 0, 0, 1)
  if (x == "2") out <- c(0, 0, 1, 0)
  if (x == "3") out <- c(0, 0, 1, 1)
  if (x == "4") out <- c(0, 1, 0, 0)
  if (x == "5") out <- c(0, 1, 0, 1)
  if (x == "6") out <- c(0, 1, 1, 0)
  if (x == "7") out <- c(0, 1, 1, 1)
  if (x == "8") out <- c(1, 0, 0, 0)
  if (x == "9") out <- c(1, 0, 0, 1)
  if (x == "a") out <- c(1, 0, 1, 0)
  if (x == "b") out <- c(1, 0, 1, 1)
  if (x == "c") out <- c(1, 1, 0, 0)
  if (x == "d") out <- c(1, 1, 0, 1)
  if (x == "e") out <- c(1, 1, 1, 0)
  if (x == "f") out <- c(1, 1, 1, 1)
  out <- as.logical(out)
  return(out)
}

hex_to_bin <- function(x) {
  x <- tolower(strsplit(x, "")[[1]])
  out <- logical()
  for (i in 1:length(x)) {
    add <- halfbit_to_bin(x[i])
    out <- c(out, add)
  }
  return(out)
}

bin_to_dec <- function(x) {
  sum((2^((length(x):1) - 1)) * x)
}

parse_bits_packet_jar <- function(x, n = Inf) {
  out <- list()
  packet_counter <- 1
  while (packet_counter <= n & length(x) >= 11 & !all(x == FALSE)) {
    out[[packet_counter]] <- parse_bits_packet(x)
    x <- x[-(1:out[[packet_counter]]$total_length)]
    packet_counter <- packet_counter + 1
  }
  return(out)
}

parse_bits_packet <- function(x){
  if (!all(is.logical(x))) {
    x <- hex_to_bin(x)
  }
  out <- list()
  out$packet_version = bin_to_dec(x[1:3])
  out$packet_type_id = bin_to_dec(x[4:6])
  out$version_sum <- out$packet_version
  if (out$packet_type_id == 4) {
    data <- x[7:length(x)]
    group_ids <- data[seq(0, (length(data) - 1), by = 5) + 1]
    last_group_start <- 5 * (min(which(group_ids == 0)) - 1) + 1
    data <- data[1:(last_group_start + 4)]
    out$n_groups <- length(data) %/% 5
    out$total_length <- 6 + out$n_groups * 5
    group_id <- (1:length(data) - 1) %/% 5 + 1
    ids <- 1:out$n_groups
    value <- logical()
    for (id in ids) {
      group_data <- data[which(group_id == id)]
      value <- c(value, group_data[2:5])
      if (group_data[1] == FALSE) break()
    }
    out$literal_value <- bin_to_dec(value)
    out$expression <- out$literal_value
  } else {
    out$length_type_id <- x[7]
    if (out$length_type_id == 1) {
      out$subpacket_count <- bin_to_dec(x[0:10 + 8])
      subpackets <- x[19:length(x)]
      out$subpackets <- parse_bits_packet_jar(subpackets, out$subpacket_count)
      out$total_length <- 18
      for (i in 1:length(out$subpackets)) {
        out$total_length <- out$total_length + out$subpackets[[i]]$total_length
      }
    } else {
      out$subpacket_length <- bin_to_dec(x[0:14 + 8])
      subpackets <- x[23 + 0:(out$subpacket_length - 1)]
      out$subpackets <- parse_bits_packet_jar(subpackets)
      out$total_length <- 23 + (out$subpacket_length - 1)
    }
    for (i in 1:length(out$subpackets)) {
      out$version_sum <- out$version_sum + out$subpackets[[i]]$version_sum
    }
    # add arguments
    out$arguments <- character(0)
    for (i in 1:length(out$subpackets)) {
      if (i == 1) {
         out$arguments <- out$subpackets[[i]]$expression
      } else {
        out$arguments <- paste(out$arguments, out$subpackets[[i]]$expression, sep = ",")
      }
    }
    out$arguments <- paste0("(", out$arguments, ")")

    # add operator
    # # in R, everything is a function call!
    if (out$packet_type_id == 0) out$operator <- "'sum'"
    if (out$packet_type_id == 1) out$operator <- "'prod'"
    if (out$packet_type_id == 2) out$operator <- "'min'"
    if (out$packet_type_id == 3) out$operator <- "'max'"
    if (out$packet_type_id == 5) out$operator <- "'>'"
    if (out$packet_type_id == 6) out$operator <- "'<'"
    if (out$packet_type_id == 7) out$operator <- "'=='"
    
    # create expression
    out$expression <- paste0(out$operator, out$arguments)

    # evaluate too??
    out$evaluation <- eval(parse(text = out$expression))
  }
  return(out)
}

tic("day 16, parts 1 and 2")
dat <- parse_bits_packet(readLines("day16/input.txt"))
toc(log = TRUE)

stopifnot(dat$version_sum == 951) # part one
stopifnot(dat$evaluation == 902198718880) # part two
