#' Extraire le carré ou rectangle autour d'une position d'une boîte dans la grille
#'
#' @param pos vecteur numérique de longueur 2 pour les 2 dimensions (x, y)
#' @param grid matrice de minesweeper
#'
#' @returns un sous-ensemble de la matrice de minesweeper 
#' @export
#'
#' @examples
#' get_around_square(c(1, 2), matrix(1:12, nrow = 3))
get_around_square <- function(pos, grid) {
  dims <- dim(grid)
  
  x <- pos[1] + -1:1
  y <- pos[2] + -1:1
  x <- x[x > 0 & x <= dims[1]]
  y <- y[y > 0 & y <= dims[2]]
  grid[x, y]
}
