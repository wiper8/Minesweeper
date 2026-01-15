source("src/indicies/i_and_positions.R")
source("src/indicies/get_around_square.R")
source("src/game_engine/compute_box_number.R")

#' révéler les cases à la suite d'un clic humain
#'
#' @param grid matrice de minesweeper
#' @param human 
#'
#' @returns matrice de minesweeper
#' @export
#'
#' @examples
#' update_grid(matrix(c(-1, -1, -1, -2, -1, -1, -1, -1, -3), nrow = 3, ncol = 3))
update_grid <- function(grid, ...) {
  for (i in which(grid == uncovered_no_mine)) {
    pos <- i_to_position(i, dim(grid))
    square <- get_around_square(pos, grid)
    grid[i] <- compute_box_number(square, ...) # calculer le chiffre à mettre
    
    if (grid[i] == 0) {
      # cliquer à nouveau automatiquement tout autour
      positions <- square_pos(pos, grid)
      reveal <- unlist(square) == covered_no_mine
      positions <- positions[reveal, , drop = FALSE]
      for (j in seq_len(nrow(positions))) {
        grid[positions[j, 1], positions[j, 2]] <- uncovered_no_mine
      }
      grid <- update_grid(grid)
    }
  }
  grid
}
