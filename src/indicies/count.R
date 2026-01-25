source(here("hp.R"))
source(here("src/indicies/square_pos.R"))
source(here("src/indicies/i_and_positions.R"))
source(here("src/indicies/get_around_square.R"))

count_unknown <- function(grid, i, values) {
  if (missing(values)) values <- count_core(grid, i)$values
  sum(!values %in% c(0:9, flag_on_mine, flag_on_no_mine))
}

count_mines_left_around <- function(grid, i, values) {
  if (missing(values)) values <- count_core(grid, i)$values
  digit <- grid[i]
  digit - sum(values %in% c(flag_on_mine, flag_on_no_mine))
}

count_core <- function(grid, i) {
  stopifnot(grid[i] %in% 0:9) # la fonction n'est pas conçue pour les cellules non chiffrées
  pos <- i_to_position(i, dim(grid))
  positions <- square_pos(pos, grid)
  values <- get_around_square(pos, grid)
  # values_pos <- square_pos(pos, grid)
  list(values = values, positions = positions)
}
