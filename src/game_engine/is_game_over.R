source("hp.R")

#' Est-ce que la partie est perdue?
#'
#' @param grid matrice de minesweeper
#' @param total_mines entier : nombre de mines au total dans la grille
#'
#' @returns nombre entier : -1 si la partie est perdue, 1 si la partie est gagnée, 0 si la partie est en cours
#' @export
#'
#' @examples
is_game_over <- function(grid, total_mines = NA, verbose = FALSE) {
  verbo <- function(grid, verbose) {
    if (verbose) {
      print(grid)
    }
  }
  
  if (any(grid == uncovered_mine)) {
    verbo(grid, verbose)
    return(-1)
  }
  
  # toutes les boîtes sans mines sont cliquées
  if (sum(grid %in% c(covered_no_mine, uncovered_no_mine, uncovered_mine, flag_on_no_mine, unknown_box)) == 0) {
    verbo(grid, verbose)
    return(1)
  }
  0
}
