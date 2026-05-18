source("hp.R")
source("src/fast_pmax.R")
source("src/indicies/i_and_positions.R")
source("src/indicies/square_pos_and_get_around_square.R")
source("src/game_engine/is_game_over.R")
source("src/game_engine/update_grid.R")

#' Appliquer une action de clic ou de flag
#'
#' @param grid matrice de minesweeper
#' @param pos vecteur numérique de longueur 2 pour les 2 dimensions (x, y)
#' @param action booléen : TRUE signifie de cliquer, FALSE de flaguer
#' @param mines_left entiern : nombre de mines restantes à placer
#' @param solved_around matrice de même dimensions que grid remplie de 0 ou 1 signifiant quelles cellules sont
#' pleinement résolues, qu'on peut désormais ignorer
#'
#' @returns liste de la grille, de si la partie est terminée et de la matrice de statuts solved_around
#' @export
#'
#' @examples
#' apply_action(matrix(-1, 3, 3), c(1, 2), FALSE, 2)
apply_action <- function(grid, pos, action, mines_left, solved_around = grid * 0 - 1, ...) {
  i <- position_to_i(pos, dim(grid))
  
  # actions sur des cases déjà révélées, ignorer
  if (grid[i] >= 0) return(list(grid, is_game_over(grid, mines_left), mines_left, solved_around))
  
  # flag
  if (!action) {
    if (isTRUE(mines_left <= 0)) stop("aucun drapeau disponible")
    tmp <- flagguer(grid, i, mines_left)
    grid <- tmp[[1]]
    mines_left <- tmp[[2]]
    solved_around <- update_solved_around(grid, solved_around, i)
  }
  
  # clic
  if (action) {
    grid <- clic(grid, i)
    tmp <- update_grid(grid, mines_left, solved_around, ...)
    grid <- tmp[[1]]
    solved_around <- tmp[[2]]
    mines_left <- tmp[[3]]
  }
  list(grid, is_game_over(grid, mines_left), mines_left, solved_around)
}

#' @param grid matrice de Minesweeper
#' @param solved_around matrice de statut de résolution des cellules
#'  -1 est une cellule sans aucune information autour, 0 est une cellule avec information autour, 1 est une cellule dont
#'  toutes les cases autour sont révélées ou flaguées
#' @param i entier : indice de la case qui vient d'être actionnée, peu importe l'action
#' @param around_too : est-ce qu'on suppose qu'on a cliqué sur les cases autour de la cellule également ?
update_solved_around <- function(grid, solved_around, i, around_too = TRUE) {
  if (solved_around[i] %in% -1:0) {
    pos <- i_to_position(i, dim(grid))
    tmp <- square_pos_and_get_around_square(pos, grid)
    positions <- tmp[[1]]
    values <- tmp[[2]]
    unknown <- !values %in% known
    n_unknown <- sum(unknown)
    dims <- dim(grid)
    
    if (n_unknown == 0) {
      solved_around[i] <- 1
      # des inconnus et des connus
    } else if (n_unknown > 0 & sum(!unknown) > 0) {
      solved_around[i] <- 0
    } else {
      return(solved_around)
    }
    # updater l'entourage aussi
    if (around_too) {
      around_pos <- positions
      keep <- positions[, 1] != pos[1] | positions[, 2] != pos[2]
      around_pos <- around_pos[keep, , drop = FALSE]
      idx <- position_to_i_mat(around_pos, dims)
      solved_around[idx] <- fast_pmax(solved_around[idx], 0)
    }
  }
  solved_around
}

flagguer <- function(grid, i, mines_left) {
  stopifnot(grid[i] %in% c(covered_no_mine, covered_mine, flag_on_mine, flag_on_no_mine, unknown_box))
  if (grid[i] == covered_mine) {
    grid[i] <- flag_on_mine
    mines_left <- mines_left - 1
    return(list(grid, mines_left))
  }
  if (grid[i] == covered_no_mine) {
    grid[i] <- flag_on_no_mine
    mines_left <- mines_left - 1
    return(list(grid, mines_left))
  }
  if (grid[i] == unknown_box) {
    grid[i] <- hypothetical_mine
    mines_left <- mines_left - 1
    return(list(grid, mines_left))
  }
  
  # retirer drapeaux
  if (grid[i] == flag_on_mine) {
    grid[i] <- covered_mine
    mines_left <- mines_left + 1
    return(list(grid, mines_left))
  }
  if (grid[i] == flag_on_no_mine) {
    grid[i] <- covered_no_mine
    mines_left <- mines_left + 1
    return(list(grid, mines_left))
  }
}

clic <- function(grid, i) {
  if (isFALSE(grid[i] %in% c(covered_no_mine, covered_mine, uncovered_no_mine, unknown_box))) browser()
  if (grid[i] == unknown_box) {
    grid[i] <- uncovered_no_mine
    return(grid)
  }
  if (grid[i] == covered_mine) {
    grid[i] <- uncovered_mine
    return(grid)
  } else {
    grid[i] <- uncovered_no_mine
    return(grid)
  }
}
