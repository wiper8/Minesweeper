
#' Convertir une position 2D dans une grille en un indice 1D
#'
#' @param pos vecteur numérique de longueur 2 pour les 2 dimensions (x, y)
#' @param dims vecteur numérique des dimensions de la grille 
#'
#' @returns vecteur numérique de la position du ie élément ciblé
#' @export
#'
#' @examples
#' position_to_i(c(3, 6), c(10, 8))
position_to_i <- function(pos, dims) {
  (pos[2] - 1) * dims[1] + pos[1]
}

#' Convertir une matrice de positions 2D dans une grille en un vecteur d'indices 1D
#'
#' @param pos_mat matrice numérique de 2 colonnes les 2 dimensions (x, y)
#' @param dims vecteur numérique des dimensions de la grille 
#'
#' @returns vecteur numérique de la position du ie élément ciblé
#' @export
#'
#' @examples
#' position_to_i_mat(matrix(c(1, 3, 10, 2, 6, 11), ncol = 2), c(10, 8))
position_to_i_mat <- function(pos_mat, dims) {
  (pos_mat[, 2] - 1) * dims[1] + pos_mat[, 1]
}

#' Convertir un indice 1D d'une position dans une grille à une position 2D
#'
#' @param i nombre entier positif
#' @param dims vecteur numérique des dimensions de la grille 
#'
#' @returns vecteur numérique de longueur 2 pour les 2 dimensions (x, y)
#' @export
#'
#' @examples
#' i_to_position(53, c(10, 8))
i_to_position <- function(i, dims) {
  col <- floor(i / dims[1])
  col <- col + (i / dims[1] != col)
  c(i - (col - 1) * dims[1], col)
}
