#' Extraire les positions dans un carré ou rectangle autour d'une boîte précise dans la grille
#'
#' @param pos vecteur numérique de longueur 2 pour les 2 dimensions (x, y)
#' @param grid matrice de minesweeper
#'
#' @returns matrice numérique de 2 colonnes correspondant à (x, y). Les valeurs sont les positions
#' @export
#'
#' @examples
#' square_pos(c(1, 2), matrix(1:12, nrow = 3))
square_pos <- function(pos, grid) {
  dims <- dim(grid)
  
  x <- pos[1] + -1:1
  y <- pos[2] + -1:1
  x <- x[x > 0 & x <= dims[1]]
  y <- y[y > 0 & y <= dims[2]]
  matrix(c(
    rep(x, length(y)),
    rep(y, each = length(x))
  ), ncol = 2)
}
