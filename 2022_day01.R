# Advent of code 2022
# day 1

# Basic functions and packages
library("dplyr")

# PART 1
# read data
my_input <- read.delim("data/day1_elves.txt", blank.lines.skip = FALSE, header = FALSE)

# ugly code
x <- my_input$V1

na_idx <- c(1, which(is.na(x)), length(x) + 1)
na_idx_length <- diff(na_idx)

my_fct <- rep(1:(length(na_idx) - 1), na_idx_length)

my_input$elve_id <- my_fct

my_smry <- my_input |> 
  dplyr::filter(!is.na(V1)) |> 
  dplyr::group_by(elve_id) |> 
  dplyr::summarise(sum = sum(V1))

# result
max(my_smry$sum)

# PART 2
my_smry |> 
  dplyr::arrange(dplyr::desc(sum)) |> 
  dplyr::slice(1:3) |> 
  dplyr::pull(sum) |> 
  sum()
