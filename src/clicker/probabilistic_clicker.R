source("src/clicker/compute_mine_probability.R")
source("src/clicker/random_clicker.R")

probabilistic_clicker <- function(grid, ...) {
  probs_grid <- compute_grid_probabilities(grid, ...)
  # try early difficult clusters that have all known information, but stays risky
  risky_lst <- risky_cluster(grid, ...)
  if (length(risky_lst) > 0) {
    riskiest <- which.max(sapply(risky_lst, function(in_cluster) min(probs_grid[in_cluster], na.rm = TRUE)))
    risky_lst[[riskiest]][!risky_lst[[riskiest]]] <- NA
    probs_grid <- probs_grid * risky_lst[[riskiest]]
  }
  next_i <- sample2(which(probs_grid == min(probs_grid, na.rm = TRUE)), 1)
  list(
    clicks = list(list(i_to_position(next_i, dim(grid)), TRUE, "probabilistic")),
    global_cache = NULL
  )
}

compute_grid_probabilities <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL,
                                       return_n_combins = FALSE, ...) {
  dims <- dim(grid)
  i_to_investigate <- find_best_i_to_investigate(grid, solved_around, click_order)
  
  mines_left_init <- mines_left
  grid_init <- grid
  solved_around_init <- solved_around

  clusters <- independant_clusters(grid, solved_around, mines_left)
  
  # recalculer les bornes précies des mines clusters
  clusters <- precise_clusters_bounds_all(grid, solved_around, mines_left, clusters, ...)

  # liste de vecteurs entiers de possibilitées. chaque vecteur est associé au cluster i
  nb_mines_possible_per_cluster <- lapply(
    clusters$clusters,
    function(clust_combins) {
      (clust_combins$bornes_mines[1]:clust_combins$bornes_mines[2])[clust_combins$possible == "TRUE"]
    }
  )
  tuples_possible <- do.call(expand.grid, nb_mines_possible_per_cluster)
  # retirer les cas où tuple + void != mines_left
  void <- mines_left - apply(tuples_possible, 1, sum)
  flush <- void > min(mines_left, sum(clusters$void$in_cluster)) | void < 0
  tuples_possible <- tuples_possible[!flush, , drop = FALSE]
  void <- void[!flush]
  n_box_void <- sum(clusters$void$in_cluster)
  
  clusters_all_probs_cache <- lapply(clusters$clusters, function(lst) {
    generate_all_probs(lst$grid, (lst$bornes_mines[1]:lst$bornes_mines[2])[lst$possible == "TRUE"], lst$solved_around, lst$in_cluster)
  })

  numerator_mine_prob <- mapply(
    function(tuple, void_i) {
      mapply(
        function(clust, tuple_i) {
          keep <- which(sapply(clust, function(x) x$mines_left) == tuple_i)
          if (length(keep) != 1) browser()
          clust[[keep]][[3]]
        },
        clusters_all_probs_cache,
        tuple
      ) |>
        rowSums()
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
            clust[[keep]]$n_combins # nb de combins de ce cluster
          },
          clusters_all_probs_cache,
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
  total_combins <- sum(numerator_weights)
  numerator_weights <- numerator_weights / total_combins
  
  probs <- mapply(function(x, w) x * w, numerator_mine_prob, numerator_weights) |>
    rowSums() / sum(numerator_weights)
  
  void_prob <- sum(numerator_weights * void / n_box_void) / sum(numerator_weights)
  
  probs[clusters$void$in_cluster] <- void_prob
  probs[grid_init %in% hp_flags] <- 1
  probs_grid <- matrix(probs, nrow = nrow(clusters$void$grid), ncol = ncol(clusters$void$grid))
  
  if (return_n_combins) {
    return(list(n_combins = total_combins, probs = probs_grid))
  }
  
  probs_grid[grid %in% known] <- NA # remplacer les cases connues par des NA pour ne pas les sélectionner
  
  if (any(is.na(probs_grid) & !grid_init %in% known)) browser()
  # pas sensé déclancher car certain_core devrait trouver tous les cas certains
  if (any(sum(probs_grid == 0 | probs_grid == 1, na.rm = TRUE))) browser()
  
  probs_grid
}

risky_cluster <- function(grid, mines_left, solved_around, click_order = NULL, ...) {
  clusters <- independant_clusters(grid, solved_around, mines_left)
  
  # recalculer les bornes précies des mines clusters
  clusters <- precise_clusters_bounds_all(grid, solved_around, mines_left, clusters, ...)
  risky_clusters <- sapply(clusters$clusters, function(lst) length(unique(lst$bornes_mines)) == 1)
  lapply(clusters$clusters[risky_clusters], function(lst) lst$in_cluster)
}

precise_clusters_bounds_all <- function(grid, solved_around, mines_left, clusters, know_possible = FALSE, ...) {
  # shortcut : si j'ai un erreur dans mon code, ce raccourci est non valide
  if (know_possible && length(clusters$clusters) == 1) {
    clusters$clusters[[1]]$bornes_mines <- c(mines_left, mines_left)
    clusters$clusters[[1]]$possible <- "TRUE"
    return(clusters)
  }
  
  # 2e raccourci
  if (know_possible && length(clusters$clusters) == 2) {
    tmp <- precise_bounds_one_cluster(clusters$clusters[[1]], grid, mines_left)
    clusters$clusters[[1]]$bornes_mines <- tmp$bornes
    clusters$clusters[[1]]$possible <- tmp$possible
    
    # raccourci : on connait d'avance certains cas
    possibilities <- rev(mines_left - (tmp$bornes[1]:tmp$bornes[2])[tmp$possible == "TRUE"])
    clusters$clusters[[2]]$bornes_mines <- c(min(possibilities), max(possibilities))
    clusters$clusters[[2]]$possible <- as.character(
      (clusters$clusters[[2]]$bornes_mines[1]:clusters$clusters[[2]]$bornes_mines[2]) %in% possibilities
    )
    return(clusters)
  }
  
  # commencer par les plus petits clusters
  tri <- sapply(clusters$clusters, function(x) sum(x$in_cluster))
  to_itr <- seq_along(clusters$clusters)[order(tri)]
  for (i in seq_along(to_itr)) {
    tmp <- precise_bounds_one_cluster(clusters$clusters[[to_itr[i]]], grid, mines_left)
    restantes_ailleurs <- mines_left - tmp$bornes[1]
    clusters$clusters[[to_itr[i]]]$bornes_mines <- tmp$bornes
    clusters$clusters[[to_itr[i]]]$possible <- tmp$possible
    for (j in tail(to_itr, -i)) {
      if (clusters$clusters[[j]]$bornes_mines[2] > restantes_ailleurs) {
        # raccourci : on peut couper des cas
        clusters$clusters[[j]]$possible[
          clusters$clusters[[j]]$bornes_mines[1]:clusters$clusters[[j]]$bornes_mines[2] > restantes_ailleurs
        ] <- "FALSE"
      }
    }
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
    if (lst$possible[left] == "TRUE") {
      possibility <- TRUE
    } else if (lst$possible[left] == "FALSE") {
      possibility <- FALSE
    } else {
      possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[left])
    }
    
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
  list(
    bornes = c(min_possible, max_possible),
    possible = as.character(possible[seq(head(which(possible), 1), tail(which(possible), 1))])
  )
}
