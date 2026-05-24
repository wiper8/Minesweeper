probabilistic_clicker <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL, to_clusterise = TRUE, ...) {
  i_to_investigate <- find_best_i_to_investigate(grid, solved_around, click_order)
  
  mines_left_init <- mines_left
  grid_init <- grid
  solved_around_init <- solved_around
  clusters <- if (to_clusterise) {
    independant_clusters(grid, solved_around, mines_left)
  } else {
    NULL
  }
  
  probs_grid <- grid * 0

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
      click_order_tmp <- rbind(click_order, positions[unknown, , drop = FALSE][mine_i, ])
      around_probs[mine_i] <- compute_mine_probability(
        grid,
        positions[unknown, , drop = FALSE][mine_i, ],
        mines_left = mines_left,
        solved_around = solved_around,
        click_order = click_order_tmp,
        to_clusterise = FALSE,
        ...
      )
    }
  }
  # TODO calculer prob d'une cellule 100% inconnue et indépendante
  # TODO choisir la plus basse prob et cliquer dessus
  
}
