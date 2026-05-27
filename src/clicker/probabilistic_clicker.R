probabilistic_clicker <- function(grid, ...) {
  probs_grid <- compute_grid_probabilities(grid, ...)
  next_i <- which.min(probs_grid)
  list(i_to_position(next_i, dim(grid)), TRUE, "probabilistic")
}

compute_grid_probabilities <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL, ...) {
  i_to_investigate <- find_best_i_to_investigate(grid, solved_around, click_order)
  
  mines_left_init <- mines_left
  grid_init <- grid
  solved_around_init <- solved_around
  clusters <- independant_clusters(grid, solved_around, mines_left, precise_bounds = TRUE)
  
  clusters_all_combins_cache <- lapply(clusters, function(lst) {
    generate_all_combins(lst$grid, lst$bornes_mines, lst$solved_around) # TODO pour les bornes de mines à gérer
  })
  browser() # TODO section ci-dessous non complétée
  
  probs_grid <- grid * NA

  # je prend une cellule avec un chiffre qui a >= 1 inconnu autour
  for (i in i_to_investigate) {
    mines_left <- mines_left_init
    grid <- grid_init
    solved_around <- solved_around_init
    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    unknown <- !values %in% known
    n_unknown <- sum(unknown)
    
    # car quand on essaie un drapeau et de le propager, ça peut arriver qu'il n'y a plus de combinaisons
    if (n_unknown == 0) next

    mines_left_around <- count_mines_left_around(grid, i, values)
    # appliquer toutes les combins de mines autour, et vérifier s'il y a une certitude
    pos_unknown <- positions[unknown, , drop = FALSE]
    
    # tester toutes les combinaisons autour de cette case, vérifier s'il y a toujours ou jamais un drapeau
    # dans les situations où on propage un flag, ça peut arriver
    if (mines_left_around < 0 || n_unknown < mines_left_around || isTRUE(mines_left < mines_left_around)) browser()
    
    if (!is.null(clusters)) {
      cluster_concerned <- sapply(clusters, function(clust) clust$solved_around[i] != -1)
      tmp <- clusters[[which(cluster_concerned)]]
      grid <- tmp$grid
      solved_around <- tmp$solved_around
      # rajouter les mines déjà flagguées des autres clusters
      if (any(!cluster_concerned)) {
        mines_left <- mines_left + sum(sapply(
          clusters[!cluster_concerned],
          function(clust) {
            sum(clust$grid == flag_on_mine)
          }
        ))
      }
    }
    
    around_probs <- rep(NA, n_unknown)
    for (mine_i in seq_len(n_unknown)) {
      next_click <- positions[unknown, , drop = FALSE][mine_i, ]
      if (is.na(probs_grid[next_click[1], next_click[2]])) {
        click_order_tmp <- rbind(click_order, next_click)

        around_probs[mine_i] <- compute_mine_probability(
          grid,
          position_to_i(next_click, dim(grid)),
          if (!is.null(clusters)) {
            clusters_all_combins_cache[[which(cluster_concerned)]]
          } else {
            clusters_all_combins_cache[[1]]
          }
        )  
      } else {
        around_probs[mine_i] <- probs_grid[next_click[1], next_click[2]]
      }
    }
    probs_grid[position_to_i_mat(positions[unknown, , drop = FALSE], dim(grid))] <- around_probs
  }
  if (any(is.na(probs_grid) & grid_init < 0)) browser()
  if (any(sum(probs_grid == 0 | probs_grid == 1, na.rm = TRUE))) browser() # pas sensé déclancher car certain_core devrait tout trouver
  probs_grid
}
