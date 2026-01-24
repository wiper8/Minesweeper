source("hp.R")
source("src/indicies/i_and_positions.R")

#' Choisir un pochain clic de manière aléatoire
#'
#' @param grid matrice de minesweeper
#' @param mines nombre entier du nombre de mines total du tableau
#'
#' @returns liste avec la position du prochain clic c(x, y), le choix de l'action (TRUE/FALSE), et la certitude
#'  ("certain", "probabilistic", "random", "impossible")
#' @export
#'
#' @examples
random_clicker <- function(grid, mines) {
  if (sum(grid %in% c(covered_no_mine, covered_mine)) == 0) {
    message("la partie est déjà terminée")
    browser()
  }
  next_i <- sample(which(grid %in% c(covered_no_mine, covered_mine)), 1)
  list(i_to_position(next_i, dim(grid)), TRUE, "random")
}
