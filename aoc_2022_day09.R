# Advent of code 2022
# day 9

# Basic functions and packages
library("dplyr")

init_knot <- function(n = 1) {
  heads <- data.frame(x = rep(NA, n), y = rep(NA, n))
  heads <- dplyr::bind_rows(data.frame(x = 0, y = 0), heads)
  heads
}

move_knot <- function(motion, x, y) {
  if (motion == "R")
    x <- x + 1
  if (motion == "L")
    x <- x - 1
  if (motion == "D")
    y <- y - 1
  if (motion == "U")
    y <- y + 1
  c(x, y)
}

move_tail <- function(pos_head, pos_tail) {
  diff <- pos_head - pos_tail
  dist <- sqrt(diff[1]^2 + diff[2]^2)
  if (dist < 2) # do not move
    return(pos_tail)
  if (all(diff == c(2, 2)))
    return(pos_head + c(-1, -1))
  if (all(diff == c(-2, 2)))
    return(pos_head + c(1, -1))
  if (all(diff == c(2, -2)))
    return(pos_head + c(-1, 1))
  if (all(diff == c(-2, -2)))
    return(pos_head + c(1, 1))
  if (diff[2] >= 2)
    return(pos_head + c(0, -1))
  if (diff[2] <= -2)
    return(pos_head + c(0, 1))
  if (diff[1] >= 2)
    return(pos_head + c(-1, 0))
  if (diff[1] <= -2)
    return(pos_head + c(1, 0))
  c(NA, NA)
}

# read data
motions0 <- read.delim("data/day9_motions.txt",
                       sep = "",
                       blank.lines.skip = FALSE,
                       header = FALSE)
motions <- rep(motions0$V1, motions0$V2)

# PART 1 (quite slow)

# init
heads <- init_knot(length(motions))
tails <- init_knot(length(motions))

for (step in 1:length(motions)) {
  
  heads[step + 1, c("x", "y")] <- move_knot(motions[step],
                                            heads$x[step],
                                            heads$y[step])
  tails[step + 1, c("x", "y")] <- move_tail(pos_head = heads[step + 1, c("x", "y")],
                                            pos_tail = tails[step, c("x", "y")])
}

tails |> 
  dplyr::distinct(x, y) |> 
  nrow()

# PART 2 (slow)
knots_list <- lapply(1:10, function(x) init_knot(length(motions)))

for (step in 1:length(motions)) {
  
  knots_list[[1]][step + 1, c("x", "y")] <- move_knot(motions[step],
                                                      knots_list[[1]]$x[step],
                                                      knots_list[[1]]$y[step])
  for (i in 2:10) {
    knots_list[[i]][step + 1, c("x", "y")] <- move_tail(
      pos_head = knots_list[[i - 1]][step + 1, c("x", "y")],
      pos_tail = knots_list[[i]][step, c("x", "y")])
  }
}

knots_list[[10]] |> 
  dplyr::distinct(x, y) |> 
  nrow()

