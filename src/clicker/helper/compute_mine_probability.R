source("src/clicker/certain_core.R")

compute_mine_probability <- function(grid, mine_i, all_combins) {
  sum(sapply(all_combins, function(sub_grid) {
    if (sub_grid[mine_i] != hypothetical_no_mine && sub_grid[mine_i] != hypothetical_mine) browser()
    sub_grid[mine_i] == hypothetical_mine
  })) / length(all_combins)
}

generate_all_probs <- function(grid, mines, solved_around, in_cluster, ...) {
  grid_tmp_propagate <- convert_grid_solution_to_human_grid(grid, solved_around, ...)
  
  lapply(mines, function(mines_left_init) {
    tmp <- generate_probs_knowing_mines(grid_tmp_propagate, mines_left_init, solved_around, in_cluster, hypothesis = 1, ...)
    list(
      mines_left = mines_left_init,
      n_combins = tmp$n_combins,
      tmp$probs
    )
  })
}

combins_to_probs <- function(combins) {
  list(
    n_combins = length(combins),
    probs = Reduce(
      `+`,
      lapply(
        combins,
        function(x) x %in% hp_flags
      )
    ) / length(combins) # / nb de combins de ce cluster
  )
}

generate_probs_knowing_mines <- function(grid_tmp_propagate, mines_left_init, solved_around, in_cluster, clusters_cache = NULL, ...) {
  grid_tmp_propagate_init <- grid_tmp_propagate

  # pour s'assurer de résoudre les cas certain car le fait de modifier mines_left peut en causer
  if (mines_left_init == 0) browser()
  tmp <- main_game_loop(grid_tmp_propagate, mines_left_init, certain_core, solved_around, ...)
  grid_tmp_propagate <- tmp[[1]]
  solved_around <- tmp[[3]]
  mines_left <- tmp[[4]]
  if (tmp[[2]] == "win") {
    return(list(
      n_combins = 1,
      probs = grid_tmp_propagate %in% hp_flags
    ))
  }
  # vérifier ici que je sample vraiment une mine possible dans le cluster
  next_i <- which(grid_tmp_propagate == -10 & solved_around == 0 & in_cluster)
  if (length(next_i) == 0) browser() # pas sensé se rendre ici
  
  clusters <- clusters_cache %||% independant_clusters(grid_tmp_propagate, solved_around, mines_left)
  clusters <- update_clusters_cache(clusters, grid_tmp_propagate_init, grid_tmp_propagate, mines_left_init - mines_left,
                                    solved_around = solved_around,
                                    mines_left = mines_left)
  # si un seul cluster
  if (length(clusters$clusters) == 1) {
    next_i <- next_i[1] # TODO mieux choisir le prochain next_i, soit avec probabilitées, le prioritise, ou le click_order
    
    dims <- dim(grid_tmp_propagate)
    mine_probs <- get_situational_probs(grid_tmp_propagate, i_to_position(next_i, dims), action = FALSE,
                                        mines_left, solved_around, in_cluster = in_cluster, ...)
    
    no_mine_probs <- get_situational_probs(grid_tmp_propagate, i_to_position(next_i, dims), action = TRUE,
                                           mines_left, solved_around, in_cluster = in_cluster, ...)
    
    list(
      n_combins = mine_probs$n_combins + no_mine_probs$n_combins,
      probs = (mine_probs$n_combins * mine_probs$probs +
                 no_mine_probs$n_combins * no_mine_probs$probs) / (mine_probs$n_combins + no_mine_probs$n_combins),
      clusters = clusters
    )
  } else {
    compute_grid_probabilities(
      grid = grid_tmp_propagate,
      mines_left = mines_left,
      solved_around = solved_around,
      return_n_combins = TRUE,
      know_possible = TRUE,
      ...
    )
  }
}

get_situational_probs <- function(grid_tmp_propagate, pos, action = FALSE,
                                  mines_left, solved_around, ...) {
  # apposer une mine temporaire
  tmp <- apply_action(grid_tmp_propagate, pos, action = action, mines_left,
                      solved_around, ...)
  # propager la partie
  tmp <- main_game_loop(tmp[[1]], tmp[[3]], certain_core, tmp[[4]], ...)
  
  if (tmp[[2]] == "win") {
    return(list(
      n_combins = 1,
      probs = tmp[[1]] %in% hp_flags
    ))
  }
  
  # TODO utiliser la cache de clusters?
  
  if (tmp[[2]] == "le clicker ne sait pu quoi faire") {
    clusters <- independant_clusters(tmp[[1]], tmp[[3]], tmp[[4]])
    if (length(clusters$clusters) == 1) {
      args <- list(...)
      args$in_cluster <- clusters$clusters[[1]]$in_cluster
      
      return(
        do.call(
          generate_probs_knowing_mines,
          append(
            args,
            list(
              grid_tmp_propagate = tmp[[1]],
              mines_left_init = tmp[[4]],
              solved_around = tmp[[3]]
            )
          )
        )
      )
    }
    return(
      compute_grid_probabilities(
        grid = tmp[[1]],
        mines_left = tmp[[4]],
        solved_around = tmp[[3]],
        return_n_combins = TRUE,
        ...
      )
    )
  }
  
  
  browser() # pas sensé déclencher
}
