source("hp.R")
source("src/indicies/i_and_positions.R")
source("src/indicies/get_around_square.R")

#' Une grille est-elle possible?
#'
#' @param grid matrice de minesweeper
#'
#' @returns booléen
#' @export
#'
#' @examples
#' is_grid_possible(matrix(c(-1, -2, 1, -1), nrow = 2))
#' is_grid_possible(matrix(c(-1, -2, 2, -1), nrow = 2))
is_grid_possible <- function(grid) {
  for (i in which(grid >= 0)) {
    pos <- i_to_position(i, dim(grid))
    square <- get_around_square(pos, grid)
    
    # TODO peut-être valider plus tard, tester plus rigoureusement
    nb_mines_so_far <- sum(square %in% c(covered_mine, uncovered_mine, flag_on_mine, hypothetical_mine))
    if (grid[i] != nb_mines_so_far) {
      if (isFALSE(nb_mines_so_far <= grid[i] && grid[i] <= nb_mines_so_far + sum(square %in% unknown_box))) {
        return(FALSE)
      }
    }
    # s'il reste 3 cases non révélées, mais qu'il reste trop de mines selon le nombre
    remaining_uncovered_boxes <- sum(square < 0)
    if (grid[i] > remaining_uncovered_boxes) return(FALSE)
  }
  TRUE
}
