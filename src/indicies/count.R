source("hp.R")
source("src/indicies/square_pos.R")
source("src/indicies/i_and_positions.R")
source("src/indicies/get_around_square.R")
source("src/indicies/square_pos_and_get_around_square.R")

count_unknown <- function(grid, i, values) {
  if (missing(values)) values <- count_core(grid, i)$values
  sum(!values %in% known)
}

count_mines_left_around <- function(grid, i, values, dims = dim(grid)) {
  if (missing(values)) {
    pos <- i_to_position(i, dims)
    values <- get_around_square(pos, grid)
  }
  digit <- grid[i]
  digit - sum(values %in% hp_flags)
}

count_core <- function(grid, i, dims = dim(grid)) {
  pos <- i_to_position(i, dims)
  tmp <- square_pos_and_get_around_square(pos, grid)
  list(values = tmp[[2]], positions = tmp[[1]])
}
