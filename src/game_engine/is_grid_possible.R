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

    # TODO peut-être valider plus tard
    if (grid[i] != sum(square %in% c(covered_mine, uncovered_mine, flag_on_mine))) return(FALSE)
  }
  TRUE
}
