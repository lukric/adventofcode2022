# Advent of code 2022
# day 4

# Basic functions and packages
library("dplyr")
library("tidyr")

a_in_b <- Vectorize(function(a1, a2, b1, b2) {
  if (a1 >= b1 && a2 <= b2)
    return(TRUE)
  FALSE
})

fully_included <- Vectorize(function(a1, a2, b1, b2) {
  direction1 <- a_in_b(a1, a2, b1, b2)
  direction2 <- a_in_b(b1, b2, a1, a2)
  return(direction1 | direction2)
})

intervals_overlap <- Vectorize(function(a1, a2, b1, b2) {
  tmp <- intersect(a1:a2, b1:b2)
  if (length(tmp) > 0)
    return(TRUE)
  FALSE
})

# read data
cleanup <- read.delim("data/day4_cleanup.txt",
                      sep = ",",
                      blank.lines.skip = FALSE,
                      header = FALSE)

names(cleanup) <- c("elve1", "elve2")

# PART 1
cleanup <- cleanup |> 
  tidyr::separate(elve1, c("elve1_lower", "elve1_upper"), remove = FALSE, convert = TRUE) |> 
  tidyr::separate(elve2, c("elve2_lower", "elve2_upper"), remove = FALSE, convert = TRUE) |> 
  dplyr::mutate(includes = fully_included(elve1_lower, elve1_upper, elve2_lower, elve2_upper))

sum(cleanup$includes)

# PART 2
cleanup <- cleanup |> 
  dplyr::mutate(overlaps = intervals_overlap(elve1_lower, elve1_upper, elve2_lower, elve2_upper))

sum(cleanup$overlaps)