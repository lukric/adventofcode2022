# Advent of code 2022
# day 6

# Basic functions and packages
library("stringr")

r_char_ct_unique <- function(x) length(unique(strsplit(x, "")[[1]]))

first_pos_of_n_distinct <- function(n_distinct, txt) {
  num_diff <- sapply(
    seq_len(stringr::str_length(txt) - n_distinct + 1), 
    function(x) r_char_ct_unique(stringr::str_sub(txt, x, x + n_distinct - 1)))
  
  min(which(num_diff == n_distinct)) + n_distinct - 1
}

# read data
signal <- readLines("data/day6_signal.txt")

# PART 1
first_pos_of_n_distinct(4, signal)

# PART 2
first_pos_of_n_distinct(14, signal)
