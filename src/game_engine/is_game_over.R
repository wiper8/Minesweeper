source("hp.R")

#' Est-ce que la partie est perdue?
#'
#' @param grid matrice de minesweeper
#'
#' @returns nombre entier : -1 si la partie est perdue, 1 si la partie est gagnée, 0 si la partie est en cours
#' @export
#'
#' @examples
is_game_over <- function(grid, verbose = FALSE) {
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
  if (sum(grid %in% c(covered_no_mine, uncovered_no_mine, uncovered_mine, flag_on_no_mine)) == 0) {
    verbo(grid, verbose)
    return(1)
  }
  0
}
