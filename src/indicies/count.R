source("hp.R")
source("src/indicies/square_pos.R")
source("src/indicies/i_and_positions.R")
source("src/indicies/get_around_square.R")

count_unknown <- function(grid, i, values) {
  if (missing(values)) values <- count_core(grid, i)$values
  sum(!values %in% known)
}

count_mines_left_around <- function(grid, i, values) {
  if (missing(values)) values <- count_core(grid, i)$values
  digit <- grid[i]
  digit - sum(values %in% c(flag_on_mine, flag_on_no_mine))
}

count_core <- function(grid, i) {
  stopifnot(grid[i] %in% 0:9) # la fonction n'est pas conçue pour les cellules non chiffrées
  pos <- i_to_position(i, dim(grid))
  tmp <- square_pos_and_get_around_square(pos, grid)
  positions <- tmp[[1]]
  values <- tmp[[2]]
  list(values = values, positions = positions)
}
