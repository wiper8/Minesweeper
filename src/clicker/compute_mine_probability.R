source("src/clicker/certain_core.R")

compute_mine_probability <- function(grid, mine_i, all_combins) {
  # TODO attention s'il faudra pondérer par la prob d'avoir N mines dans mines_left (surtout pour les cas de bornes de mines variables)
  sum(sapply(all_combins, function(sub_grid) {
    if (sub_grid[mine_i] != hypothetical_no_mine && sub_grid[mine_i] != hypothetical_mine) browser()
    sub_grid[mine_i] == hypothetical_mine
  })) / length(all_combins)
}

generate_all_combins <- function(grid, mines, solved_around, in_cluster, ...) {
  grid_tmp_propagate <- convert_grid_solution_to_human_grid(grid, solved_around, ...)
  
  lapply(mines, function(mines_left_init) {
    # pour s'assurer de résoudre les cas certain car le fait de modifier mines_left peut en causer
    tmp <- main_game_loop(grid_tmp_propagate, mines_left_init, certain_core, solved_around, hypothesis = 1)
    grid_tmp_propagate <- tmp[[1]]
    solved_around <- tmp[[3]]
    mines_left <- tmp[[4]]
    if (tmp[[2]] == "win") {
      return(list(mines_left = mines_left_init, list(grid_tmp_propagate)))
    }
    # vérifier ici que je sample vraiment une mine possible dans le cluster
    next_i <- which(grid_tmp_propagate == -10 & solved_around == 0 & in_cluster)
    if (length(next_i) == 0) {
      return(NULL)
    }
    if (length(next_i) == 0) {
      if (mines_left != mines_left_init) browser()
      return(list(mines_left = mines_left, list(grid_tmp_propagate)))
    }
    next_i <- next_i[1] # TODO mieux choisir le prochain next_i, soit avec probabilitées, le prioritise, ou le click_order

    mine_combins <- get_situational_combins(grid_tmp_propagate, i_to_position(next_i, dim(grid)), action = FALSE,
                                            mines_left_init, solved_around, hypothesis = 1, in_cluster = in_cluster, ...)
    
    no_mine_combins <- get_situational_combins(grid_tmp_propagate, i_to_position(next_i, dim(grid)), action = TRUE,
                                               mines_left_init, solved_around, hypothesis = 1, in_cluster = in_cluster, ...)
    
    list(mines_left = mines_left_init, append(mine_combins, no_mine_combins))
  })
}

get_situational_combins <- function(grid_tmp_propagate, pos, action = FALSE,
                                    mines_left, solved_around, hypothesis = 1, ...) {
  # apposer une mine temporaire
  tmp <- apply_action(grid_tmp_propagate, pos, action = action, mines_left,
                      solved_around, hypothesis = hypothesis, ...)
  # propager la partie
  tmp <- main_game_loop(tmp[[1]], tmp[[3]], certain_core, tmp[[4]], hypothesis = hypothesis, ...)
  
  if (tmp[[2]] == "win") {
    return(tmp[1])
  }
  if (tmp[[2]] == "le clicker ne sait pu quoi faire") {
    return(generate_all_combins(tmp[[1]], tmp[[4]], tmp[[3]], ...)[[1]][[2]])
  }
  browser() # pas sensé déclencher
}
