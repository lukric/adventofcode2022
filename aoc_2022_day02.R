# Advent of code 2022
# day 2

# Basic functions and packages
library("dplyr")

shape_value <- Vectorize(function(x) {
  # returns the value of the shapes
  out <- NA
  if (x %in% c("A", "X")) # Rock
    out <- 1
  if (x %in% c("B", "Y")) # Paper
    out <- 2
  if (x %in% c("C", "Z")) # Scissors
    out <- 3
  
  out
})

rps <- Vectorize(function(op, me) {
  # recode to only use A B C for both
  if (me %in% "X")
    me <- "A"
  if (me %in% "Y")
    me <- "B"
  if (me %in% "Z")
    me <- "C"
  
  # outcomes of rock, paper, scissors
  if (op == me)
    return(3)
  if (op == "A" & me == "B")
    return(6)
  if (op == "A" & me == "C")
    return(0)
  if (op == "B" & me == "A")
    return(0)
  if (op == "B" & me == "C")
    return(6)
  if (op == "C" & me == "A")
    return(6)
  if (op == "C" & me == "B")
    return(0)
  NA
})

rps_decrypt <- Vectorize(function(op, outcome) {
  # returns choice based on opponent input and given outcome (win/draw/lose)
  if (outcome == "Y")
    return(op)
  if (op == "A" & outcome == "X")
    return("C")
  if (op == "B" & outcome == "X")
    return("A")
  if (op == "C" & outcome == "X")
    return("B")
  if (op == "A" & outcome == "Z")
    return("B")
  if (op == "B" & outcome == "Z")
    return("C")
  if (op == "C" & outcome == "Z")
    return("A")
  NA
})

# read data
my_strat <- read.delim("data/day2_strategy.txt",
                       blank.lines.skip = FALSE,
                       header = FALSE,
                       sep = " ")

names(my_strat) <- c("opponent", "me")

# PART 1
# calculations
my_strat <- my_strat |> 
  dplyr::mutate(shape_val = shape_value(me),
                round_outcome = rps(opponent, me),
                round_score = shape_val + round_outcome)

my_strat |> 
  dplyr::summarise(sum(round_score))

# PART 2
my_strat <- my_strat |> 
  dplyr::mutate(my_pick = rps_decrypt(opponent, me),
                shape_val = shape_value(my_pick),
                round_outcome = rps(opponent, my_pick),
                round_score = shape_val + round_outcome)

my_strat |> 
  dplyr::summarise(sum(round_score))


