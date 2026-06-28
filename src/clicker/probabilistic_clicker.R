source("src/clicker/random_clicker.R")
source("src/clicker/helper/is_cluster_island.R")
source("src/clicker/helper/compute_mine_probability.R")

probabilistic_clicker <- function(grid, clusters_cache = NULL, ...) {
  probs_grid_lst <- NULL

  clusters <- clusters_cache %||% independant_clusters(grid, ...)
  if (length(clusters$clusters) > 1) {
    islands <- is_cluster_island(grid, clusters)

    if (sum(islands) > 0) {

      ### TODO peut-être en cas de island non certain sur le nombre de mines, il y a moyen de pondérer les probs, 
      # mais ça semble assez complexe d'obtenir les vraies probabilités, donc je flush pour l'instant
      clusters <- cluster_from_draft(grid, clusters_cache = clusters, ...)
      certain_island <- sapply(
        clusters$clusters,
        function(lst) {
          if (any(lst$possible == "NA")) browser()
          sum(lst$possible == "TRUE") == 1
        }
      )
      islands <- islands & certain_island
      ###
      
      if (sum(islands) > 0) {
        keep <- Reduce(
          `+`,
          lapply(clusters$clusters[islands], function(lst) lst$in_cluster)
        ) > 0
        grid[!keep] <- -5
        # trouver les probs seulement pour les îles
        clusters_all_probs_cache <- lapply(clusters$clusters[islands], function(lst) {
          if (sum(lst$possible == "TRUE") != 1) browser() # pas implémenté
          args <- list(...)
          args$grid <- lst$grid
          args$in_cluster <- lst$in_cluster
          args$mines_left <- NULL
          args$hypothesis <- NULL
          args$mines <- (lst$bornes_mines[1]:lst$bornes_mines[2])[lst$possible == "TRUE"]
          args$solved_around <- lst$solved_around
          do.call(
            generate_all_probs,
            args
          )
        })

        min_prob_per_island <- mapply(
          function(lst, island_probs) {
            if (length(island_probs) > 1) browser() # erreur d'implémentation si ça déclenche
            min(island_probs[[1]]$probs[lst$in_cluster & !lst$grid %in% known], na.rm = TRUE)
          },
          clusters$clusters[islands],
          clusters_all_probs_cache
        )
        i_riskiest_clust <- which.max(min_prob_per_island)
        clusters_all_probs_cache <- clusters_all_probs_cache[[i_riskiest_clust]]
        probs_grid_lst <- clusters_all_probs_cache[[1]]
        to_overwrite_to_NA <- !clusters$clusters[islands][[i_riskiest_clust]]$in_cluster | grid %in% known
        probs_grid_lst$probs[to_overwrite_to_NA] <- NA
      }
    }
  }
  if (is.null(probs_grid_lst)) probs_grid_lst <- compute_grid_probabilities(grid, clusters_cache = clusters_cache, ...)

  next_i <- sample2(which(probs_grid_lst$probs == min(probs_grid_lst$probs, na.rm = TRUE)), 1)

  list(
    clicks = list(list(i_to_position(next_i, dim(grid)), TRUE, "probabilistic")),
    global_cache = NULL,
    clusters_cache = probs_grid_lst$clusters
  )
}

compute_grid_probabilities <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL,
                                       return_n_combins = FALSE, clusters_cache = NULL, ...) {
  dims <- dim(grid)
  i_to_investigate <- find_best_i_to_investigate(grid, solved_around, click_order)

  mines_left_init <- mines_left
  grid_init <- grid
  solved_around_init <- solved_around

  clusters <- cluster_from_draft(grid, solved_around, mines_left, clusters_cache, ...)
  if (is.null(clusters)) browser() # pas sensé déclencher
  if (length(clusters$clusters) == 0) { # raccourci
    void <- mines_left
    n_box_void <- sum(clusters$void$in_cluster)

    total_combins <- choose(n_box_void, void)

    probs <- grid * 0

    void_prob <- sum(void / n_box_void)

    probs[clusters$void$in_cluster] <- void_prob
    probs[grid_init %in% hp_flags] <- 1

    probs_grid <- matrix(probs, nrow = nrow(clusters$void$grid), ncol = ncol(clusters$void$grid))

    if (return_n_combins) {
      return(list(n_combins = total_combins, probs = probs_grid, clusters = clusters))
    }

    probs_grid[grid %in% known] <- NA # remplacer les cases connues par des NA pour ne pas les sélectionner

    if (any(is.na(probs_grid) & !grid_init %in% known)) browser()
    # pas sensé déclancher car certain_core devrait trouver tous les cas certains
    if (any(sum(probs_grid == 0 | probs_grid == 1, na.rm = TRUE))) browser()
    return(list(probs = probs_grid, clusters = clusters))
  }

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
    generate_all_probs(lst$grid, (lst$bornes_mines[1]:lst$bornes_mines[2])[lst$possible == "TRUE"], lst$solved_around, lst$in_cluster, ...)
  })

  numerator_mine_prob <- mapply(
    function(tuple, void_i) {
      mapply(
        function(clust, tuple_i) {
          keep <- which(sapply(clust, function(x) x$mines_left) == tuple_i)
          if (length(keep) != 1) browser()
          clust[[keep]]$probs
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

  tmp <- mapply(function(x, w) x * w, numerator_mine_prob, numerator_weights)

  probs <- rowSums(tmp) / sum(numerator_weights)

  void_prob <- sum(numerator_weights * void / n_box_void) / sum(numerator_weights)

  probs[clusters$void$in_cluster] <- void_prob
  probs[grid_init %in% hp_flags] <- 1

  probs_grid <- matrix(probs, nrow = nrow(clusters$void$grid), ncol = ncol(clusters$void$grid))

  if (return_n_combins) {
    return(list(n_combins = total_combins, probs = probs_grid, clusters = clusters))
  }

  probs_grid[grid %in% known] <- NA # remplacer les cases connues par des NA pour ne pas les sélectionner

  if (any(is.na(probs_grid) & !grid_init %in% known)) browser()
  # pas sensé déclancher car certain_core devrait trouver tous les cas certains
  if (any(sum(probs_grid == 0 | probs_grid == 1, na.rm = TRUE))) browser()

  list(probs = probs_grid, clusters = clusters)
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

  # update les bornes du void
  clusters$void$bornes_mines[1] <- max(
    0,
    clusters$void$bornes_mines[1],
    mines_left - ifelse(length(clusters$clusters) == 0, 0, sum(sapply(clusters$clusters, function(x) x$bornes_mines[2])))
  )
  clusters$void$bornes_mines[2] <- min(
    clusters$void$bornes_mines[2],
    mines_left - ifelse(length(clusters$clusters) == 0, 0, sum(sapply(clusters$clusters, function(x) x$bornes_mines[1])))
  )
  clusters$void$possible <- rep("TRUE", clusters$void$bornes_mines[2] - clusters$void$bornes_mines[1] + 1)
  clusters
}

precise_bounds_one_cluster <- function(lst, grid, mines_left) {
  if (mines_left == 0) browser() # pas sensé déclancher
  trials <- lst$bornes_mines[1]:lst$bornes_mines[2]
  # commencer au milieu ou au premier TRUE
  if (any(lst$possible == "TRUE")) {
    left <- head(which(lst$possible == "TRUE"), 1)
  } else {
    left <- ceiling(length(trials) / 2)
  }
  left_trials <- rev(seq_along(trials)[seq_len(left)])
  if (length(trials) > 1) {
    right_trials <- seq_along(trials)[(left + 1):length(trials)]
  } else {
    right_trials <- c()
  }

  min_possible <- NA
  max_possible <- NA
  possible <- rep(NA, length(trials))

  activate_shortcut <- FALSE
  for (left in left_trials) {
    if (lst$possible[left] == "TRUE") {
      possibility <- TRUE
    } else if (lst$possible[left] == "FALSE") {
      possibility <- FALSE
    } else {
      if (activate_shortcut) possibility <- FALSE
      # je ne PEUX PAS supposer qu'il y a un bloc continu de TRUE consécutifs. Des cas existent où ce n'est pas vrai
      else possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[left])
      if (possibility == "not enough mines") {
        activate_shortcut <- TRUE
        possibility <- FALSE
      } else if (is.character(possibility)) possibility <- FALSE # car le test_trial_shortcut a trouvé un raccourci
    }
    
    if (possibility) {
      possible[left] <- TRUE
      min_possible <- min(min_possible, trials[left], na.rm = TRUE)
      max_possible <- max(max_possible, trials[left], na.rm = TRUE)
    } else {
      possible[left] <- FALSE
    }
  }

  activate_shortcut <- FALSE
  for (left in right_trials) {
    if (lst$possible[left] == "TRUE") {
      possibility <- TRUE
    } else if (lst$possible[left] == "FALSE") {
      possibility <- FALSE
    } else {
      if (activate_shortcut) possibility <- FALSE
      else possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[left])
      # je ne PEUX PAS supposer qu'il y a un bloc continu de TRUE consécutifs. Des cas existent où ce n'est pas vrai
      if (possibility == "too many mines") {
        activate_shortcut <- TRUE
        possibility <- FALSE
      } else if (is.character(possibility)) possibility <- FALSE # car le test_trial_shortcut a trouvé un raccourci
    }

    if (possibility) {
      possible[left] <- TRUE
      min_possible <- min(min_possible, trials[left], na.rm = TRUE)
      max_possible <- max(max_possible, trials[left], na.rm = TRUE)
    } else {
      possible[left] <- FALSE
    }
  }
  if (length(which(possible)) == 0) browser()
  list(
    bornes = c(min_possible, max_possible),
    possible = as.character(possible[seq(head(which(possible), 1), tail(which(possible), 1))])
  )
}
