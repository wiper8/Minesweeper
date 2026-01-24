source("hp.R")
source("src/indicies/i_and_positions.R")
source("src/game_engine/is_game_over.R")
source("src/game_engine/update_grid.R")

#' Appliquer une action de clic ou de flag
#'
#' @param grid matrice de minesweeper
#' @param pos vecteur numérique de longueur 2 pour les 2 dimensions (x, y)
#' @param action booléen : TRUE signifie de cliquer, FALSE de flaguer
#' @param solved_around matrice de même dimensions que grid remplie de 0 ou 1 signifiant quelles cellules sont
#' pleinement résolues, qu'on peut désormais ignorer
#'
#' @returns liste de la grille, de si la partie est terminée et de la matrice de statuts solved_around
#' @export
#'
#' @examples
#' apply_action(matrix(-1, 3, 3), c(1, 2), FALSE)
apply_action <- function(grid, pos, action, solved_around = matrix(0, nrow = nrow(grid), ncol = ncol(grid)), ...) {
  i <- position_to_i(pos, dim(grid))
  
  # actions sur des cases déjà révélées, ignorer
  if (grid[i] >= 0) return(list(grid, is_game_over(grid)), solved_around)
  
  # flag
  if (!action) {
    grid <- flagguer(grid, i)
  }
  
  # clic
  if (action) {
    grid <- clic(grid, i)
    tmp <- update_grid(grid, solved_around, ...)
    grid <- tmp[[1]]
    solved_around <- tmp[[2]]
  }
  
  solved_around <- update_solved_around(grid, solved_around)
  
  list(grid, is_game_over(grid), solved_around)
}

update_solved_around <- function(grid, solved_around) {
  for (i in which(solved_around == 0)) {
    pos <- i_to_position(i, dim(grid))
    positions <- square_pos(pos, grid)
    
    values <- get_around_square(pos, grid)
    values_pos <- square_pos(pos, grid)
    unknown <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine)
    n_unknown <- sum(unknown)
    if (n_unknown == 0) {
      solved_around[i] <- 1
    }
  }
  solved_around
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
