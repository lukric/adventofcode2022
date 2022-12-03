# Advent of code 2022
# day 3

# Basic functions and packages
library("dplyr")
library("stringr")
library("purrr")


find_matching_chars <- function(x, y) {
  x1 <- stringr::str_split(x, "")
  y1 <- stringr::str_split(y, "")
  
  sapply(seq_along(x1), function(x) paste0(intersect(x1[[x]], y1[[x]]), collapse = ""))
}

find_double_item <- function(x) {
  l <- stringr::str_length(x)
  x1 <- stringr::str_sub(x, 1, l/2)
  x2 <- stringr::str_sub(x, l/2 + 1, l)
  
  find_matching_chars(x1, x2)
}

find_multiple <- function(x) {
  purrr::reduce(x, find_matching_chars)
}

item_value <- function(x) {
  values <- 1:52
  names(values) <- c(letters, LETTERS)
  values[x]
}

# read data
rucksacks <- read.delim("data/day3_rucksack.txt",
                              blank.lines.skip = FALSE,
                              header = FALSE)
names(rucksacks) <- "contents"

# PART 1
rucksacks <- rucksacks |> 
  dplyr::mutate(item_in_both = find_double_item(contents),
                item_val = item_value(item_in_both))

sum(rucksacks$item_val)

# PART 2
groups <- rucksacks |> 
  dplyr::mutate(id = ceiling(dplyr::row_number()/3)) |> 
  dplyr::group_by(id) |> 
  dplyr::summarise(item_identical = find_multiple(contents)) |> 
  dplyr::mutate(item_val = item_value(item_identical))

sum(groups$item_val)
