source("hp.R")
source("src/indicies/i_and_positions.R")
source("src/game_engine/is_game_over.R")

#' Appliquer une action de clic ou de flag
#'
#' @param grid matrice de minesweeper
#' @param pos vecteur numérique de longueur 2 pour les 2 dimensions (x, y)
#' @param action booléen : TRUE signifie de cliquer, FALSE de flaguer
#'
#' @returns liste de la grille et de si la partie est terminée
#' @export
#'
#' @examples
#' apply_action(matrix(-1, 3, 3), c(1, 2), FALSE)
apply_action <- function(grid, pos, action, ...) {
  i <- position_to_i(pos, dim(grid))
  
  # actions sur des cases déjà révélées, ignorer
  if (grid[i] >= 0) return(list(grid, is_game_over(grid)))
  
  # flag
  if (!action) {
    grid <- flagguer(grid, i)
  }
  
  # clic
  if (action) {
    grid <- clic(grid, i)
    grid <- update_grid(grid, ...)
  }
  
  list(grid, is_game_over(grid))
}

flagguer <- function(grid, i) {
  stopifnot(grid[i] %in% c(covered_no_mine, covered_mine, flag_on_mine, flag_on_no_mine))
  
  # TODO améliorer et accélérer probablement
  grid[i] <- ifelse(
    grid[i] == covered_mine,
    flag_on_mine,
    ifelse(
      grid[i] == covered_no_mine,
      flag_on_no_mine, 
      ifelse(
        grid[i] == flag_on_mine,
        covered_mine,
        covered_no_mine
      )
    )
  )
  grid
}

clic <- function(grid, i) {
  stopifnot(grid[i] %in% c(covered_no_mine, covered_mine))
  grid[i] <- ifelse(grid[i] == covered_mine, uncovered_mine, uncovered_no_mine)
  grid
}
