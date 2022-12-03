# Advent of code 2022
# day 1

# Basic functions and packages
library("dplyr")

# PART 1
# read data
elves_inventory <- read.delim("data/day1_elves.txt",
                              blank.lines.skip = FALSE,
                              header = FALSE)

# ugly code
x <- elves_inventory$V1

na_idx <- c(1, which(is.na(x)), length(x) + 1)
na_idx_length <- diff(na_idx)

elves_inventory$elve_id <- rep(1:(length(na_idx) - 1), na_idx_length)

elves_cals <- elves_inventory |> 
  dplyr::filter(!is.na(V1)) |> 
  dplyr::group_by(elve_id) |> 
  dplyr::summarise(sum = sum(V1))

# result
max(elves_cals$sum)

# PART 2
elves_cals |> 
  dplyr::arrange(dplyr::desc(sum)) |> 
  dplyr::slice(1:3) |> 
  dplyr::pull(sum) |> 
  sum()
