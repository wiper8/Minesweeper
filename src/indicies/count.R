source("hp.R")
source("src/indicies/square_pos.R")
source("src/indicies/i_and_positions.R")
source("src/indicies/get_around_square.R")
source("src/indicies/square_pos_and_get_around_square.R")

count_unknown <- function(grid, i, values) {
  if (missing(values)) values <- count_core(grid, i)$values
  sum(!values %in% known)
}

count_mines_left_around <- function(grid, i, values) {
  if (missing(values)) values <- count_core(grid, i)$values
  digit <- grid[i]
  digit - sum(values %in% hp_flags)
}

count_core <- function(grid, i) {
  pos <- i_to_position(i, dim(grid))
  tmp <- square_pos_and_get_around_square(pos, grid)
  positions <- tmp[[1]]
  values <- tmp[[2]]
  list(values = values, positions = positions)
}
