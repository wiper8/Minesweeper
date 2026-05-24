compute_mine_probability <- function(grid, mine_pos, mines_left, solved_around, ...) {
  mines_left_init <- mines_left
  solved_around_init <- solved_around

  # TODO finir la fonction
  grid_tmp_propagate <- convert_grid_solution_to_human_grid(grid, solved_around, ...)

  # apposer une mine temporaire
  tmp <- apply_action(grid_tmp_propagate, mine_pos, action = FALSE, mines_left,
                      solved_around, hypothesis = 1, ...)
  grid_tmp_propagate <- tmp[[1]]
  mines_left <- tmp[[3]]
  solved_around <- tmp[[4]]
  # compter le nombre de simulations possibles
  combins_with_mine <- count_combins_possible(grid_tmp_propagate, mines_left, solved_around, ...)

  mines_left <- mines_left_init
  solved_around <- solved_around_init

  # retirer la mine et poser une non-mines temporaire
  tmp <- apply_action(grid_tmp_propagate, mine_pos, action = TRUE, mines_left,
                      solved_around, hypothesis = 1, ...)
  grid_tmp_propagate <- tmp[[1]]
  mines_left <- tmp[[3]]
  solved_around <- tmp[[4]]

  # compter le nombre de simulations possibles
  combins_without_mine <- count_combins_possible(grid_tmp_propagate, mines_left, solved_around, ...)

  combins_with_mine / (combins_with_mine + combins_without_mine)
}

count_combins_possible <- function(grid, mines_left, solved_around, ...) {
  # TODO
  # propager la partie
  tmp <- main_game_loop(grid, mines_left, certain_core, solved_around, hypothesis = 1, ...)
  if (tmp[[2]] == "win") { # TODO vérifier que c'est le seul bon critère d'arrêt
    return(1)
  }
  if (tmp[[2]] == "le clicker ne sait pu quoi faire") { # TODO vérifier que c'est le seul bon critère d'arrêt
    return(1)
  }
}
