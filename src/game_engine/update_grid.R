source("src/indicies/i_and_positions.R")
source("src/indicies/get_around_square.R")
source("src/indicies/square_pos.R")
source("src/game_engine/is_game_over.R")
source("src/game_engine/compute_box_number.R")

#' révéler les cases à la suite d'un clic humain
#'
#' @param grid matrice de minesweeper
#' @param mines_left entier : nombre de mines restantes à placer
#' @param solved_around matrice de même dimensions que grid remplie de 0 ou 1 signifiant quelles cellules sont
#' pleinement résolues, qu'on peut désormais ignorer
#'
#' @returns liste de matrice de minesweeper, de matrice du statut des cases pleinement résolues incluant les 8 autour,
#'  et du nombre de mines restantes à placer
#' @export
#'
#' @examples
#' update_grid(matrix(c(-1, -1, -1, -2, -1, -1, -1, -1, -3), nrow = 3, ncol = 3), NA)
update_grid <- function(grid, mines_left, solved_around = matrix(0, nrow = nrow(grid), ncol = ncol(grid)), ...) {
  for (i in which(grid == uncovered_no_mine)) {
    pos <- i_to_position(i, dim(grid))
    square <- get_around_square(pos, grid)
    grid[i] <- compute_box_number(square, ...) # calculer le chiffre à mettre
    
    if (grid[i] == 0) {
      solved_around[i] <- 1
      # cliquer à nouveau automatiquement tout autour
      positions <- square_pos(pos, grid)
      reveal <- unlist(square) == covered_no_mine
      positions <- positions[reveal, , drop = FALSE]
      for (j in seq_len(nrow(positions))) {
        grid[positions[j, 1], positions[j, 2]] <- uncovered_no_mine
        if (solved_around[positions[j, 1], positions[j, 2]] == -1) solved_around[positions[j, 1], positions[j, 2]] <- 0
      }
      tmp <- update_grid(grid, mines_left, solved_around)
      grid <- tmp[[1]]
      solved_around <- tmp[[2]]
      mines_left <- tmp[[3]]
    }
  }
  if (is_game_over(grid) == 1) {
    place_flag <- grid == covered_mine
    mines_left <- mines_left - sum(place_flag)
    grid[place_flag] <- flag_on_mine # flagger automatiquement toutes les mines quand la partie est terminée
  }
  list(grid, solved_around, mines_left)
}
