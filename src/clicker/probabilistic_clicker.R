source("src/clicker/compute_mine_probability.R")
probabilistic_clicker <- function(grid, ...) {
  probs_grid <- compute_grid_probabilities(grid, ...)
  next_i <- sample(which(probs_grid == min(probs_grid, na.rm = TRUE)), 1)
  list(
    clicks = list(list(i_to_position(next_i, dim(grid)), TRUE, "probabilistic")),
    global_cache = NULL
  )
}

compute_grid_probabilities <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL, ...) {
  dims <- dim(grid)
  i_to_investigate <- find_best_i_to_investigate(grid, solved_around, click_order)
  
  mines_left_init <- mines_left
  grid_init <- grid
  solved_around_init <- solved_around
  probs_grid <- grid * NA

  clusters <- independant_clusters(grid, solved_around, mines_left)
  
  # recalculer les bornes précies des mines clusters
  clusters <- precise_clusters_bounds_all(grid, solved_around, mines_left, clusters)

  # liste de vecteurs entiers de possibilitées. chaque vecteur est associé au cluster i
  nb_mines_possible_per_cluster <- lapply(
    clusters$clusters,
    function(clust_combins) {
      (clust_combins$bornes_mines[1]:clust_combins$bornes_mines[2])[clust_combins$possible]
    }
  )
  tuples_possible <- do.call(expand.grid, nb_mines_possible_per_cluster)
  # retirer les cas où tuple + void != mines_left
  void <- mines_left - apply(tuples_possible, 1, sum)
  flush <- void > min(mines_left, sum(clusters$void$in_cluster)) | void < 0
  tuples_possible <- tuples_possible[!flush, , drop = FALSE]
  void <- void[!flush]
  n_box_void <- sum(clusters$void$in_cluster)
  
  clusters_all_combins_cache <- lapply(clusters$clusters, function(lst) {
    generate_all_probs(lst$grid, (lst$bornes_mines[1]:lst$bornes_mines[2])[lst$possible], lst$solved_around, lst$in_cluster)
  })
  # browser()

  numerator_mine_prob <- mapply(
    function(tuple, void_i) {
      mapply(
        function(clust, tuple_i) {
          keep <- which(sapply(clust, function(x) x$mines_left) == tuple_i)
          if (length(keep) != 1) browser()
          clust[[keep]][[2]]
        },
        clusters_all_combins_cache,
        tuple
      ) |>
        rowSums() |>
        matrix(nrow = dims[1]) # TODO retirer car ralenti, mais augmente interpretabilité
    },
    split(tuples_possible, seq_len(nrow(tuples_possible))),
    void,
    SIMPLIFY = FALSE
  )
  numerator_weights <- mapply(
    function(tuple, void_i) {
      c(
        mapply(
          function(clust, tuple_i) {
            keep <- which(sapply(clust, function(x) x$mines_left) == tuple_i)
            if (length(keep) != 1) browser()
            
            length(clust[[keep]][[2]]) # nb de combins de ce cluster
          },
          clusters_all_combins_cache,
          tuple
        ),
        choose(n_box_void, void_i)
      ) |>
        prod()
    },
    split(tuples_possible, seq_len(nrow(tuples_possible))),
    void
  )
  # pour éviter overflow
  numerator_weights <- numerator_weights / sum(numerator_weights)
  
  probs <- mapply(function(x, w) x * w, numerator_mine_prob, numerator_weights) |>
    rowSums() / sum(numerator_weights)
  
  void_prob <- sum(numerator_weights * void / n_box_void) / sum(numerator_weights)
  
  probs[clusters$void$in_cluster] <- void_prob
  
  probs_grid <- matrix(probs, nrow = nrow(clusters$void$grid), ncol = ncol(clusters$void$grid))

  probs_grid[grid %in% known] <- NA # remplacer les cases connues par des NA pour ne pas les sélectionner

  if (any(is.na(probs_grid) & !grid_init %in% known)) browser()
  # pas sensé déclancher car certain_core devrait trouver tous les cas certains
  if (any(sum(probs_grid == 0 | probs_grid == 1, na.rm = TRUE))) browser()

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
  if (length(which(possible)) == 0) browser()
  list(bornes = c(min_possible, max_possible), possible = possible[seq(head(which(possible), 1), tail(which(possible), 1))])
}

clusters_combins_to_probs <- function(clusters, clusters_all_combins_cache, mines_total) {
  
  
}

