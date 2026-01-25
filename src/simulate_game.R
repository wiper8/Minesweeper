source("hp.R")
source("src/game_engine/update_grid.R")
source("src/game_engine/apply_action.R")
source("src/game_engine/is_game_over.R")
source("src/indicies/i_and_positions.R")
source("src/clicker/random_first_click.R")

#' Simuler une partie de Minsweeper
#'
#' @param mines nombre entier : nombre de mines total dans la grille
#' @param dims vecteur de deux entiers : nombre de cases horizontalement et verticalement de la grille
#' @param clicker fonction de type clicker permettant d'effectuer des choix quant aux prochaines actions à prendre
#'
#' @returns liste de la grille et de "win" ou "lost" selon la situation
#' @export
#'
#' @examples
#' simulate_game(10, c(10, 8), random_clicker)
simulate_game <- function(mines, dims = c(17, 9), clicker) {
  grid <- matrix(NA, nrow = dims[1], ncol = dims[2])
  # TODO changer pour une meilleure fonction, il est possible que commencer au centre ou aux coins est avantageux
  # faiblement
  first_click <- random_first_click(dims)
  tmp <- init_grid_after_first_click(grid, first_click, mines)
  grid <- tmp[[1]]
  if (is_game_over(grid) == 1) return(list(grid, "win"))
  
  solved_around <- matrix(0, nrow = nrow(grid), ncol = ncol(grid))
  main_game_loop(grid, mines, clicker, solved_around = solved_around)
}

init_grid_after_first_click <- function(grid, pos, mines) {
  dims <- dim(grid)
  grid[pos[1], pos[2]] <- uncovered_no_mine
  # put mines in the game
  grid[sample(setdiff(seq_len(prod(dims)), position_to_i(pos, dims)), mines)] <- covered_mine
  grid[is.na(grid)] <- covered_no_mine
  update_grid(grid)
}

main_game_loop <- function(grid, mines, clicker, solved_around, ...) {
  repeat {
      # clicker
      tmp <- clicker(grid, total_mines = mines, solved_around = solved_around, ...)
      if (is.null(tmp)) return(list(grid, "le clicker ne sait pu quoi faire", solved_around))
      
      tmp2 <- apply_action(grid, tmp[[1]], tmp[[2]], solved_around = solved_around, ...)
      grid <- tmp2[[1]]
      solved_around <- tmp2[[3]]
      if (tmp2[[2]] == 1) return(list(grid, "win", solved_around))
      if (tmp2[[2]] == -1) return(list(grid, "lost", solved_around))
  }
}
