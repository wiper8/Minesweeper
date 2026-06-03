source("src/clicker/compute_mine_probability.R")

probabilistic_clicker <- function(grid, ...) {
  # temporairement, mettre des probs à 0.1, d'autres à 0.9
  probs_grid <- compute_grid_probabilities(grid, ...)
  # temporairement, sélectionner une boîte aléatoirement au lieu de directement le plus bas
  next_i <- sample(which(probs_grid == min(probs_grid, na.rm = TRUE)), 1)
  list(list(i_to_position(next_i, dim(grid)), TRUE, "probabilistic"))
}

compute_grid_probabilities <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL, ...) {
  i_to_investigate <- find_best_i_to_investigate(grid, solved_around, click_order)
  
  mines_left_init <- mines_left
  grid_init <- grid
  solved_around_init <- solved_around
  probs_grid <- grid * NA
  
  clusters <- independant_clusters(grid, solved_around, mines_left)
  
  # recalculer les bornes précies des mines clusters
  clusters <- precise_clusters_bounds_all(grid, solved_around, mines_left, clusters)

  # TODO vérifier si donne les bons résultats (bon nb mines)
  clusters_all_combins_cache <- lapply(clusters$clusters, function(lst) {
    generate_all_combins(lst$grid, (lst$bornes_mines[1]:lst$bornes_mines[2])[lst$possible], lst$solved_around, lst$in_cluster)
  })
  browser()
  clusters_dependancies(clusters, clusters_all_combins_cache, mines_left)

  # void probs
  # TODO approximatif pour l'instant
  void_prob <- mean(clusters$void$bornes_mines) / sum(clusters$void$in_cluster)
  probs_grid[clusters$void$in_cluster] <- void_prob

  # je prend une cellule avec un chiffre qui a au moins un inconnu autour
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
    
    cluster_concerned <- sapply(clusters$clusters, function(clust) clust$solved_around[i] != -1)
    tmp <- clusters$clusters[[which(cluster_concerned)]]
    grid <- tmp$grid
    solved_around <- tmp$solved_around
    # rajouter les mines déjà flagguées des autres clusters
    if (any(!cluster_concerned)) {
      mines_left <- mines_left + sum(sapply(
        clusters$clusters[!cluster_concerned],
        function(clust) {
          sum(clust$grid == flag_on_mine)
        }
      ))
    }
    
    around_probs <- rep(NA, n_unknown)
    for (mine_i in seq_len(n_unknown)) {
      next_click <- positions[unknown, , drop = FALSE][mine_i, ]
      if (is.na(probs_grid[next_click[1], next_click[2]])) {
        click_order_tmp <- rbind(click_order, next_click)
        
        around_probs[mine_i] <- compute_mine_probability(
          grid,
          position_to_i(next_click, dim(grid)),
          # traitement temporaire pour simplifier tous les cas selon nb de mines dans le cluster
          lapply(clusters_all_combins_cache[[which(cluster_concerned)]], function(lst) lst[[2]]) |>
            unlist(recursive = FALSE)
        )
      } else {
        around_probs[mine_i] <- probs_grid[next_click[1], next_click[2]]
      }
    }
    probs_grid[position_to_i_mat(positions[unknown, , drop = FALSE], dim(grid))] <- around_probs
  }
  if (any(is.na(probs_grid) & !grid_init %in% known)) browser()
  # pas sensé déclancher car certain_core devrait trouver tous les cas certains
  if (any(sum(probs_grid == 0 | probs_grid == 1, na.rm = TRUE))) browser()
  # TODO ajout temporaire pour simplifier les probs vu qu'elles ne sont pas pondérées
  probs_grid[!is.na(probs_grid) & probs_grid != 0 & probs_grid != 1 & probs_grid == min(probs_grid, na.rm = TRUE)] <- 0.1
  probs_grid[!is.na(probs_grid) & probs_grid != 0 & probs_grid != 1 & probs_grid > min(probs_grid, na.rm = TRUE)] <- 0.9
  
  probs_grid
}

precise_clusters_bounds_all <- function(grid, solved_around, mines_left, clusters) {
  for (i in seq_along(clusters$clusters)) {
    tmp <- precise_bounds_one_cluster(clusters$clusters[[i]], grid, mines_left)
    clusters$clusters[[i]]$bornes_mines <- tmp$bornes
    clusters$clusters[[i]]$possible <- tmp$possible
  }
  clusters
}

precise_bounds_one_cluster <- function(lst, grid, mines_left) {
  trials <- lst$bornes_mines[1]:lst$bornes_mines[2]
  left <- 1
  min_possible <- NA
  max_possible <- NA
  possible <- rep(NA, length(trials))
  while (left <= length(trials)) {
    possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[left])
    if (possibility) {
      possible[left] <- TRUE
      if (is.na(min_possible)) {
        min_possible <- trials[left]
      } else {
        min_possible <- min(min_possible, trials[left])
      }
      if (is.na(max_possible)) {
        max_possible <- trials[left]
      } else {
        max_possible <- max(max_possible, trials[left])
      }
    } else {
      possible[left] <- FALSE
    }
    left <- left + 1
  }
  list(bornes = c(min_possible, max_possible), possible = possible[seq(head(which(possible), 1), tail(which(possible), 1))])
}

clusters_dependancies <- function(clusters, clusters_all_combins_cache, mines_total) {
  # liste de vecteurs entiers de possibilitées. chaque vecteur est associé au cluster i
  nb_mines_possible_per_cluster <- lapply(
    clusters_all_combins_cache,
    function(clust_combins) {
      sapply(clust_combins, function(x) x$mines_left)
    }
  )
  tuples_possible <- do.call(expand.grid, nb_mines_possible_per_cluster)

  # retirer les cas où tuple + void != mines_total
  void <- mines_total - apply(tuples_possible, 1, sum)
  flush <- void > min(mines_total, sum(clusters$void$in_cluster)) | void < 0
  tuples_possible <- tuples_possible[!flush, , drop = FALSE]
  void <- void[!flush]
  n_box_void <- sum(clusters$void$in_cluster)
  # ajouter le void
  mapply(
    function(tuple, void_i) {
      # TODO vérifier la logique de pondération et de x != unkwnown_box
      numerator <- mapply(
        function(clust, tuple_i) {
          keep <- which(sapply(clust, function(x) x$mines_left) == tuple_i)
          if (length(keep) != 1) browser()
          
          Reduce(
            `+`,
            lapply(clust[[keep]][[2]], function(x) x %in% hp_flags)
          ) * choose(n_box_void, void_i)
        },
        clusters_all_combins_cache,
        tuple
      )
      denominator <- mapply(
        function(clust, tuple_i) {
          keep <- which(sapply(clust, function(x) x$mines_left) == tuple_i)
          if (length(keep) != 1) browser()
          
          # TODO compléter et vérifier, calculer soit length() pour nb de combins, pondérer probablement par
          Reduce(
            `+`,
            lapply(clust[[keep]][[2]], function(x) x != unknown_box)
          ) * choose(n_box_void, void_i)
        },
        clusters_all_combins_cache,
        tuple
      )
    },
    split(tuples_possible, seq_len(nrow(tuples_possible))),
    void
  )
}

