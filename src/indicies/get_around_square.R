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
get_around_square <- function(pos, grid, dims = dim(grid)) {
  x <- pos[1] + -1:1
  y <- pos[2] + -1:1
  x <- x[x > 0 & x <= dims[1]]
  y <- y[y > 0 & y <= dims[2]]
  grid[x, y]
}

get_around_cross <- function(pos, grid, dims = dim(grid)) {
  corners <- matrix(c(NA, 1, NA, 1, 1, 1, NA, 1, NA), nrow = 3)

  x <- pos[1] + -1:1
  y <- pos[2] + -1:1
  x <- x[x > 0 & x <= dims[1]]
  y <- y[y > 0 & y <= dims[2]]
  subgrid <- grid[x, y]
  if (length(x) == 3 && length(y) == 3) return(subgrid * corners)

  if (length(x) < 3) {
    if (x[1] == 1) { # première rangée
      x <- c(2, 3)
    } else { # dernière rangée
      x <- c(1, 2)
    }
  } else {
    x <- 1:3
  }

  if (length(y) < 3) {
    if (y[1] == 1) { # première colonne
      y <- c(2, 3)
    } else { # dernière colonne
      y <- c(1, 2)
    }
  } else {
    y <- 1:3
  }
  
  subgrid * corners[x, y]
}
