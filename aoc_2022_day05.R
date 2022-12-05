# Advent of code 2022
# day 1

# Basic functions and packages
library("dplyr")

clean_crates <- function(x) {
  x <- rev(trimws(x))
  x[x != ""]
}

move_crates <- function(crates, n, from, to, revert = TRUE) {
  # specify which to move
  to_move <- tail(crates[[from]], n)
  if (revert)
    to_move <- rev(to_move)
  # add at new spot (reverse)
  crates[[to]] <- c(crates[[to]], to_move)
  # remove from old spot
  crates[[from]] <- head(crates[[from]], -n)
  crates
}

# read data
crates0 <- read.fwf("data/day5_1_crates.txt", rep(4, 9),
                     blank.lines.skip = FALSE,
                     header = FALSE)

moves0 <- readLines("data/day5_2_movings.txt")

crates0 <- crates0[-nrow(crates0), ]
crates_orig <- lapply(as.list(crates0), clean_crates)

moves2 <- stringr::str_replace(moves0, "move ", "") |> 
  stringr::str_replace("from ", "") |> 
  stringr::str_replace("to ", "")

moves <- read.delim(text = moves2, sep = " ", header = FALSE)
names(moves) <- c("n_move", "from", "to")

# PART 1
crates <- crates_orig
for (i in seq_len(nrow(moves))) {
  crates <- move_crates(crates = crates, 
                        n = moves[i, "n_move"], 
                        from = moves[i, "from"], 
                        to =  moves[i, "to"])
}

sapply(crates, tail, 1) |> 
  stringr::str_replace(pattern = "\\[", "") |> 
  stringr::str_replace(pattern = "\\]", "") |> 
  paste0(collapse = "")

# PART 2
crates <- crates_orig
for (i in seq_len(nrow(moves))) {
  crates <- move_crates(crates = crates, 
                        n = moves[i, "n_move"], 
                        from = moves[i, "from"], 
                        to =  moves[i, "to"],
                        revert = FALSE)
}

sapply(crates, tail, 1) |> 
  stringr::str_replace(pattern = "\\[", "") |> 
  stringr::str_replace(pattern = "\\]", "") |> 
  paste0(collapse = "")
