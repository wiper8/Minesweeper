source("hp.R")
source("src/game_engine/update_grid.R")
source("src/game_engine/apply_action.R")
source("src/game_engine/is_game_over.R")
source("src/indicies/i_and_positions.R")
source("src/clicker/random_first_click.R")

#' Simuler une partie de Minsweeper
#'
#' @param total_mines nombre entier : nombre de total_mines total dans la grille
#' @param dims vecteur de deux entiers : nombre de cases horizontalement et verticalement de la grille
#' @param clicker fonction de type clicker permettant d'effectuer des choix quant aux prochaines actions à prendre
#'
#' @returns liste de la grille et de "win" ou "lost" selon la situation
#' @export
#'
#' @examples
#' simulate_game(10, c(10, 8), random_clicker)
simulate_game <- function(total_mines, dims = c(17, 9), clicker) {
  grid <- matrix(NA, nrow = dims[1], ncol = dims[2])
  # TODO changer pour une meilleure fonction, il est possible que commencer au centre ou aux coins est avantageux
  # faiblement
  first_click <- random_first_click(dims)
  tmp <- init_grid_after_first_click(grid, first_click, total_mines)
  grid <- tmp[[1]]
  mines_left <- tmp[[3]]
  if (is_game_over(grid) == 1) return(list(grid, "win"))
  
  solved_around <- matrix(-1, nrow = nrow(grid), ncol = ncol(grid))
  solved_around <- update_solved_around(grid, solved_around, position_to_i(first_click, dim(grid)))
  main_game_loop(grid, total_mines, clicker, solved_around = solved_around)
}

init_grid_after_first_click <- function(grid, pos, total_mines) {
  dims <- dim(grid)
  grid[pos[1], pos[2]] <- uncovered_no_mine
  # put mines in the game
  grid[sample(setdiff(seq_len(prod(dims)), position_to_i(pos, dims)), total_mines)] <- covered_mine
  grid[is.na(grid)] <- covered_no_mine
  update_grid(grid, total_mines)
}

main_game_loop <- function(grid, mines_left, clicker, solved_around, ...) {
  repeat {
    # clicker
    tmp <- clicker(grid, mines_left = mines_left, solved_around = solved_around, ...)
    if (isTRUE(all.equal(tmp, "impossible"))) return(list(grid, "partie impossible", solved_around))
    if (is.null(tmp)) return(list(grid, "le clicker ne sait pu quoi faire", solved_around))
    
    
    tmp2 <- apply_action(grid, tmp[[1]], tmp[[2]], mines_left, solved_around = solved_around, ...)
    grid <- tmp2[[1]]
    solved_around <- tmp2[[3]]
    if (tmp2[[2]] == 1) return(list(grid, "win", solved_around))
    if (tmp2[[2]] == -1) return(list(grid, "lost", solved_around))
  }
}
