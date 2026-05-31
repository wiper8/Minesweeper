source("src/fast_apply.R")
source("src/indicies/count.R")
source("src/game_engine/is_grid_possible.R")
source("src/clicker/is_mine_propagation_possible.R")
source("src/clicker/find_best_i_to_investigate.R")

certain_core <- function(grid, mines_left, solved_around, hypothesis, ...) {
  tmp <- can_flag_all_around(grid, mines_left, solved_around)
  if (!is.null(tmp)) return(tmp)
  tmp <- can_click_all_around(grid, solved_around)
  if (!is.null(tmp)) return(tmp)
  tmp <- can_deduce_pattern(grid, mines_left, solved_around, hypothesis, ...)
  if (!is.null(tmp)) return(tmp)
  NULL # retourner NULL si on ne sait pas quelle action certain prendre.
}

can_flag_all_around <- function(grid, mines_left, solved_around) {
  for (i in which(grid > 0 & solved_around == 0)) {
    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    n_unknown <- count_unknown(grid, i, values)
    if (n_unknown > 0 && count_mines_left_around(grid, i, values) == n_unknown) {
      unknown <- !values %in% known
      if (isTRUE(mines_left - sum(unknown) < 0)) return("impossible")
      return(apply(positions[unknown, , drop = FALSE], 1, function(pos) list(pos, FALSE), simplify = FALSE))
    }
  }
  NULL
}

can_click_all_around <- function(grid, solved_around) {
  for (i in which(grid > 0 & solved_around == 0)) {
    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    n_unknown <- count_unknown(grid, i, values)
    if (n_unknown > 0 && count_mines_left_around(grid, i, values) == 0) {
      unknown <- !values %in% known
      return(apply(positions[unknown, , drop = FALSE], 1, function(pos) list(pos, TRUE), simplify = FALSE))
    }
  }
  NULL
}

can_deduce_pattern <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL, to_clusterise = TRUE, ...) {
  impossible <- TRUE # pour hypothesis = 2
  reached_prop <- FALSE
  i_to_investigate <- find_best_i_to_investigate(grid, solved_around, click_order)
  
  mines_left_init <- mines_left
  grid_init <- grid
  solved_around_init <- solved_around
  
  clusters <- if (to_clusterise) {
    independant_clusters(grid, solved_around, mines_left)
  } else {
    NULL
  }
  
  # car possible qu'on soit bloqué ET qu'il n'y ait aucun i_to_investigate disponible, qu'il faut guess random
  if (length(i_to_investigate) == 0 && hypothesis != 2) impossible <- FALSE
  
  global_cache <- list() # cache des cas POSSIBLES, pas confirmés
  
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
    reached_prop <- TRUE
    
    mines_left_around <- count_mines_left_around(grid, i, values)
    # appliquer toutes les combins de mines autour, et vérifier s'il y a une certitude
    pos_unknown <- positions[unknown, , drop = FALSE]
    
    # tester toutes les combinaisons autour de cette case, vérifier s'il y a toujours ou jamais un drapeau
    # dans les situations où on propage un flag, ça peut arriver
    if (mines_left_around < 0 || n_unknown < mines_left_around || isTRUE(mines_left < mines_left_around)) return("impossible")
    
    if (!is.null(clusters)) {
      cluster_concerned <- sapply(clusters$clusters, function(clust) clust$in_cluster[i] == 1)
      if (!is.logical(cluster_concerned) || length(cluster_concerned) == 0) browser()
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
    }
    
    combins <- combn(n_unknown, mines_left_around)
    cache <- rep(NA, ncol(combins))
    
    for (mine_i in seq_len(n_unknown)) {
      mines_has_mine_i <- fast_apply(combins, 2, function(comb) mine_i %in% comb)
      
      # je me questionne : parmi les mines restantes autour,
      # si je ne flag JAMAIS une cellule et que toutes les combinaisons ne sont pas possibles,
      # c'est que je dois la flagguer
      possible <- which_combins_possible(
        grid,
        combins[, !mines_has_mine_i, drop = FALSE],
        pos_unknown,
        solved_around = solved_around,
        mines_left = mines_left,
        cache = cache[!mines_has_mine_i],
        global_cache = global_cache,
        click_order = click_order,
        to_clusterise = FALSE,
        ...
      )
      for (k in which(!is.na(possible))) {
        not_in_cache_yet <- !any(sapply(
          global_cache,
          function(cache_lst) {
            all(
              cache_lst[[1]] %in%
                position_to_i_mat(pos_unknown[combins[, !mines_has_mine_i, drop = FALSE][, k], , drop = FALSE], dim(grid))
            )
          }
        ))
        if (not_in_cache_yet) {
          global_cache[[length(global_cache) + 1]] <- list(
            position_to_i_mat(
              pos_unknown[
                combins[, !mines_has_mine_i, drop = FALSE][, k],
                ,
                drop = FALSE
              ],
              dim(grid)
            ),
            possible[k]
          )
        }
      }
      possible <- possible[!is.na(possible)]
      cache[which(!mines_has_mine_i)[seq_along(possible)]] <- possible
      if (hypothesis != 2 && all(!possible)) return(list(list(pos_unknown[mine_i, ], FALSE)))
      if (any(possible)) {
        impossible <- FALSE
      }
      if (hypothesis == 2 && !impossible) {
        # on a trouvé un cas possible, on va dire que le clicker ne sait pu quoi faire pour l'instant
        # fonctionne uniquement en mode hypothesis, et parce que certain_core va retourner NULL, et que dans
        # main_game_loop, va trouver une exception "le clicker ne sait pu quoi faire" et renvoyer ça à
        # is_mine_propagation_possible qui va dire TRUE
        return(NULL)
      }
      
      # si à l'inverse, je flag la cellule, et que toutes les situations sont impossibles, c'est qu'il n'y a pas de mine!
      # donc la cliquer
      possible <- which_combins_possible(
        grid,
        combins[, mines_has_mine_i, drop = FALSE],
        pos_unknown,
        solved_around = solved_around,
        mines_left = mines_left,
        cache = cache[mines_has_mine_i],
        global_cache = global_cache,
        click_order = click_order,
        to_clusterise = FALSE,
        ...
      )
      for (k in which(!is.na(possible))) {
        not_in_cache_yet <- !any(sapply(
          global_cache,
          function(cache_lst) {
            all(
              cache_lst[[1]] %in%
                position_to_i_mat(pos_unknown[combins[, mines_has_mine_i, drop = FALSE][, k], , drop = FALSE], dim(grid))
            )
          }
        ))
        if (not_in_cache_yet) {
          global_cache[[length(global_cache) + 1]] <- list(
            position_to_i_mat(
              pos_unknown[
                combins[, mines_has_mine_i, drop = FALSE][, k],
                ,
                drop = FALSE
              ],
              dim(grid)
            ),
            possible[k]
          )
        }
      }
      possible <- possible[!is.na(possible)]
      cache[which(mines_has_mine_i)[seq_along(possible)]] <- possible
      if (hypothesis != 2 && all(!possible)) return(list(list(pos_unknown[mine_i, ], TRUE)))
      if (any(possible)) {
        impossible <- FALSE
      }
      if (hypothesis == 2 && !impossible) {
        return(NULL) # voir commentaire précédent
      }
      if (hypothesis == 2 && isTRUE(all(!cache))) return("impossible")
    }
  }
  tmp <- deduce_unknown_boxes(grid_init, mines_left_init)
  if (any(is.na(tmp[[1]][[1]]))) browser()
  if (!is.null(tmp)) return(tmp)
  if (isTRUE(all.equal(tmp, "impossible"))) return("impossible")
  if (hypothesis != 2 && reached_prop && impossible) browser() # pas sensé etre impossible si on n'est pas en exploration
  if (hypothesis == 2 && reached_prop) return("impossible")
  NULL # ne sait pas quoi faire
}

deduce_unknown_boxes <- function(grid, mines_left) {
  if (is.na(mines_left)) return(NULL)
  known_boxes <- grid %in% known
  if (mines_left < 0) return("impossible")
  if (mines_left == 0) return(lapply(which(!known_boxes), function(i) list(i_to_position(i, dim(grid)), TRUE)))
  no_info_boxes <- sum(!known_boxes)
  if (no_info_boxes < mines_left) return("impossible")
  if (no_info_boxes == mines_left) return(list(list(i_to_position(which(!known_boxes)[1], dim(grid)), FALSE)))
  
  solved_around <- init_solved_around(grid)
  # on a aucune information sur les boîtes
  if (all(solved_around != 0) && no_info_boxes > mines_left) {
    return(NULL)
  }
  
  res <- are_no_mine_in_void(grid, solved_around, mines_left)
  if (!isFALSE(res)) {
    return(res)
  }
  
  NULL
}

are_no_mine_in_void <- function(grid, solved_around, mines_left) {
  known_boxes <- grid %in% known
  
  # petit raccourci : s'il y a moins de mines que le nombre de boîtes à découvrir
  if (mines_left <= sum(solved_around == 0 & !known_boxes)) {
    clusters <- independant_clusters(grid, solved_around, mines_left)
    
    if (length(clusters$clusters) == 0 && mines_left == 0) {
      next_i <- which(clusters$void$in_cluster)[1]
      return(list(list(i_to_position(next_i, dim(grid)), TRUE)))
    }
    if (length(clusters$clusters) == 0) {
      return(FALSE) # que du void avec des mines
    }
    if (all(!clusters$void$in_cluster)) {
      return(FALSE) # pas de void
    }
    
    clusters_bornes_min_precises <- precise_clusters_bounds_min_shortcut(grid, solved_around, mines_left, clusters)
    
    # SHORTCUT
    # si toutes les mines sont assurément dans les clusters, je peux cliquer dans le vide
    if (sum(clusters_bornes_min_precises) == mines_left) {
      next_i <- which(clusters$void$in_cluster)[1]
      return(list(list(i_to_position(next_i, dim(grid)), TRUE)))
    }
  }
  FALSE
}


precise_clusters_bounds_min_shortcut <- function(grid, solved_around, mines_left, clusters) {
  # TODO
  # tester tout de suite avec mines_left
  if (length(clusters$clusters) == 1) {
    bornes_mines <- clusters$clusters[[1]]$bornes_mines
    # si ce n'est pas possible, on sait que le shortcut dans deduce_unknown_boxes ne déclanchera pas
    possibility <- test_trial(grid, mines_left, clusters$clusters[[1]]$in_cluster, mines_left)
    if (possibility) {
      # vérifier si c'est bel et bien la bornes min à mines_left, si oui, on pognera le shortcut dans deduce_unknown_boxes
      trial <- mines_left - 1
      while (trial >= 0 && trial >= bornes_mines[1]) {
        possibility <- test_trial(grid, mines_left, clusters$clusters[[1]]$in_cluster, trial)
        if (possibility) {
          # volontairement retourner un nombre erroné pour pas que le SHORTCUT dans are_no_mine_in_void déclenche
          return(trial)
        } else {
          trial <- trial - 1
        }
      }
      return(mines_left)
    } else {
      # volontairement retourner un nombre erroné pour pas que le SHORTCUT dans are_no_mine_in_void déclenche
      return(mines_left - 1)
    }
  }
  
  res <- numeric(length(clusters$clusters))
  cumul_min_shortcut <- 0
  for (i in seq_along(res)) {
    lst <- clusters$clusters[[i]]
    # cluster d'une seule case connue
    if (sum(lst$in_cluster) == 1) {
      res[i] <- 0
    } else {
      trials <- lst$bornes_mines[1]:lst$bornes_mines[2]
      left <- 1
      right <- length(trials)
      min_possible <- NA
      while (left <= right) {
        possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[left])
        if (possibility) {
          min_possible <- trials[left]
          break
        } else {
          left <- left + 1
        }
      }
      res[i] <- min_possible
    }
    cumul_min_shortcut <- cumul_min_shortcut + res[i]
    if (cumul_min_shortcut > mines_left) {
      # volontairement retourner un nombre erroné pour pas que le SHORTCUT dans are_no_mine_in_void déclenche
      return(cumul_min_shortcut)
    }
  }
  res
}

#' Retourne si une proposition de mines est possible (génère une partie sans problèmes)
which_combins_possible <- function(grid, combins, pos_unknown, solved_around, mines_left,
                                   cache = rep(NA, ncol(combins)), global_cache = list(),
                                   click_order = NULL, ...) {
  mines_left_init <- mines_left
  solved_around_init <- solved_around
  possible <- rep(NA, ncol(combins))
  for (i in seq_len(ncol(combins))) {
    mines_left <- mines_left_init
    solved_around <- solved_around_init
    if (!is.na(cache[i])) {
      possible[i] <- cache[i]
      next
    }
    combin <- combins[, i]
    find_in_global_cache <- if (length(global_cache) == 0) {
      NULL
    } else {
      which(sapply(
        global_cache,
        function(cache_lst) {
          all(
            cache_lst[[1]] %in% position_to_i_mat(pos_unknown[combin, , drop = FALSE], dim(grid))
          )
        }
      ))
    }
    if (!is.null(find_in_global_cache) && any(find_in_global_cache)) {
      possible[i] <- all(sapply(global_cache[find_in_global_cache], function(x) x[[2]]))
      next
    }
    # supposer des mines
    # puis propager avec certitude, et voir si c'est possible
    grid_tmp_propagate <- convert_grid_solution_to_human_grid(grid, solved_around, ...)
    i_to_flag <- position_to_i_mat(pos_unknown[combin, , drop = FALSE], dim(grid))
    i_to_click <- position_to_i_mat(pos_unknown[-combin, , drop = FALSE], dim(grid))
    i_to_click <- i_to_click[grid[i_to_click] %in% hp_to_hypo_no_mine]
    
    for (j in i_to_flag) {
      tmp <- apply_action(grid_tmp_propagate, i_to_position(j, dim(grid_tmp_propagate)), action = FALSE, mines_left,
                          solved_around, hypothesis = 2, ...)
      grid_tmp_propagate <- tmp[[1]]
      mines_left <- tmp[[3]]
      solved_around <- tmp[[4]]
    }
    
    for (j in i_to_click) {
      j_pos <- i_to_position(j, dim(grid_tmp_propagate))
      tmp <- apply_action(grid_tmp_propagate, j_pos, action = TRUE, mines_left,
                          solved_around, hypothesis = 2, ...)
      grid_tmp_propagate <- tmp[[1]]
      mines_left <- tmp[[3]]
      solved_around <- tmp[[4]]
      click_order <- rbind(click_order, j_pos)
    }
    
    possible[i] <- is_mine_propagation_possible(grid_tmp_propagate, mines_left = mines_left,
                                                solved_around = solved_around, click_order = click_order, ...)
    if (possible[i]) break # early exist cause the calling function (which_combins_possible) checks for all FALSE
  }
  possible
}

convert_grid_solution_to_human_grid <- function(grid, solved_around, ...) {
  human_grid <- grid
  human_grid[solved_around == -1 & !grid %in% known] <- unknown_box
  human_grid[human_grid %in% hp_to_hypo_no_mine] <- unknown_box
  human_grid
}
