source("hp.R")

#' Calculer le chiffre autour d'une boîte sans mine dans la grille
#'
#' @param square matrice de minesweeper
#' @param human booléen : si le calcul est humain, on doit compter les drapeaux mals placé, sinon les exclure
#'
#' @returns nombre entier du nombre de mines
#' @export
#'
#' @examples
#' grid <- matrix(c(-2, -5, -6, -1, -1, -1, -1, -1, -1), ncol = 3)
#' compute_box_number(grid)
#' compute_box_number(grid, human = TRUE)
compute_box_number <- function(square, human = FALSE, ...) {
  sum(square %in% c(covered_mine, flag_on_mine, uncovered_mine) | (square == flag_on_no_mine) & human)
}
