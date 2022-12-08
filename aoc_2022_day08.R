# Advent of code 2022
# day 8

# Basic functions and packages
library("dplyr")

what_is_visible <- function(x, reverse = FALSE) {
  # x is a numeric verctor
  biggest <- x[1]
  vis <- rep("-", length(x))
  for (i in 2:length(x)) {
    vis[i] <- ifelse(x[i] > biggest, "x", "-")
    biggest <- max(biggest, x[i])
  }
  if (reverse)
    vis <- rev(vis)
  which(vis == "x")
}

view_distance <- function(x) {
  dist <- which(x[-1] >= x[1])
  if (length(dist) == 0)
    dist <- length(x) - 1
  min(dist)
}

tree_scenic_score <- function(i, j, m) {
  # i, j coordiantes of the tree in matrix m
  # look up
  u <- view_distance(m[i:1, j])
  # look down
  d <- view_distance(m[i:nrow(m), j])
  # look left
  l <- view_distance(m[i, j:1])
  # look right
  r <- view_distance(m[i, j:ncol(m)])
  # scenic score
  u * d * l * r
}

# read data
tmp <- readLines("data/day8_trees.txt")

trees <- read.fwf("data/day8_trees.txt", 
                  widths = rep(1, max(stringr::str_count(tmp))),
                  blank.lines.skip = FALSE,
                  header = FALSE) |> 
  as.matrix()

# PART 1
# on the plan: "-" means not visible, "x" means visible
visible_plan <- matrix(data = "-", nrow = nrow(trees), ncol = ncol(trees))
visible_plan[1, ] <- "x"
visible_plan[, 1] <- "x"
visible_plan[nrow(visible_plan), ] <- "x"
visible_plan[, ncol(visible_plan)] <- "x"

for (i in 2:(nrow(trees) - 1)) {
  x <- trees[, i]
  y <- rev(trees[, i])
  
  a <- what_is_visible(x)
  b <- what_is_visible(y, reverse = TRUE)
  
  visible_plan[c(a, b), i] <- "x"
}

for (i in 2:(ncol(trees) - 1)) {
  x <- trees[i, ]
  y <- rev(trees[i, ])
  
  a <- what_is_visible(x)
  b <- what_is_visible(y, reverse = TRUE)
  
  visible_plan[i, c(a, b)] <- "x"
}

sum(visible_plan == "x")

# PART 2
score <- sapply(seq_len(nrow(trees)),
                function(i) sapply(seq_len(ncol(trees)), 
                                   function(j) tree_scenic_score(i, j, trees)))
max(score)
