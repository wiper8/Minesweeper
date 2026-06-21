source("src/fast_apply.R")
source("src/indicies/count.R")
source("src/game_engine/is_grid_possible.R")
source("src/clicker/helper/update_cache.R")
source("src/clicker/helper/is_mine_propagation_possible.R")
source("src/clicker/helper/find_best_i_to_investigate.R")

certain_core <- function(grid, mines_left, solved_around, ...) {
  tmp <- can_flag_all_around(grid, mines_left, solved_around, ...)
  if (!is.null(tmp$clicks)) return(tmp)
  tmp <- can_click_all_around(grid, mines_left, solved_around, ...)
  if (!is.null(tmp$clicks)) return(tmp)
  tmp <- can_deduce_pattern(grid, mines_left, solved_around, ...)
  if (!is.null(tmp$clicks)) return(tmp)
  tmp # retourner NULL si on ne sait pas quelle action certain prendre.
}

can_flag_all_around <- function(grid, mines_left, solved_around, global_cache = list(), clusters_cache = NULL, ...) {
  grid_init <- grid
  mines_left_init <- mines_left
  solved_around_init <- solved_around
  for (i in which(grid > 0 & solved_around == 0)) {
    grid <- grid_init
    mines_left <- mines_left_init
    solved_around <- solved_around_init

    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    n_unknown <- count_unknown(grid, i, values)
    if (n_unknown > 0 && count_mines_left_around(grid, i, values) == n_unknown) {
      unknown <- !values %in% known
      if (isTRUE(mines_left - sum(unknown) < 0)) return(list(clicks = "impossible", global_cache = global_cache,
                                                             clusters_cache = clusters_cache))
      
      # tenter de mettre les mines pour vérifier si possible
      proposal <- list(
        clicks = apply(positions[unknown, , drop = FALSE], 1, function(pos) list(pos, FALSE, "certain"), simplify = FALSE),
        global_cache = global_cache,
        clusters_cache = clusters_cache
      )
      for (new_action in proposal$clicks) {
        tmp2 <- apply_action(grid, new_action[[1]], new_action[[2]], mines_left, solved_around = solved_around, ...)
        grid <- tmp2[[1]]
        mines_left <- tmp2[[3]]
        solved_around <- tmp2[[4]]
      }
      if (!is_grid_possible(grid)) return(list(clicks = "impossible",
                                               global_cache = global_cache,
                                               clusters_cache = clusters_cache))
      
      return(proposal)
    }
  }
  list(clicks = NULL, global_cache = global_cache, clusters_cache = clusters_cache)
}

can_click_all_around <- function(grid, mines_left, solved_around, global_cache = list(), clusters_cache = NULL, ...) {
  grid_init <- grid
  mines_left_init <- mines_left
  solved_around_init <- solved_around
  for (i in which(grid > 0 & solved_around == 0)) {
    grid <- grid_init
    mines_left <- mines_left_init
    solved_around <- solved_around_init

    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    n_unknown <- count_unknown(grid, i, values)
    if (n_unknown > 0 && count_mines_left_around(grid, i, values) == 0) {
      unknown <- !values %in% known

      # tenter de mettre les mines pour vérifier si possible
      proposal <- list(
        clicks = apply(positions[unknown, , drop = FALSE], 1, function(pos) list(pos, TRUE, "certain"), simplify = FALSE),
        global_cache = global_cache,
        clusters_cache = clusters_cache
      )
      for (new_action in proposal$clicks) {
        tmp2 <- apply_action(grid, new_action[[1]], new_action[[2]], mines_left, solved_around = solved_around, ...)
        grid <- tmp2[[1]]
        mines_left <- tmp2[[3]]
        solved_around <- tmp2[[4]]
      }
      if (!is_grid_possible(grid)) return(list(clicks = "impossible",
                                               global_cache = global_cache,
                                               clusters_cache = clusters_cache))

      return(proposal)
    }
  }
  list(clicks = NULL, global_cache = global_cache, clusters_cache = clusters_cache)
}

can_deduce_pattern <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL, to_clusterise = TRUE, 
                               in_cluster = NULL, global_cache = list(), clusters_cache = NULL, ...) {
  dims <- dim(grid)
  impossible <- TRUE # pour hypothesis = 2
  reached_prop <- FALSE
  i_to_investigate <- find_best_i_to_investigate(grid, solved_around, click_order)
  
  mines_left_init <- mines_left
  grid_init <- grid
  solved_around_init <- solved_around
  
  # car possible qu'on soit bloqué ET qu'il n'y ait aucun i_to_investigate disponible, qu'il faut guess random
  if (length(i_to_investigate) == 0 && hypothesis != 2) impossible <- FALSE
  
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
    if (mines_left_around < 0 || n_unknown < mines_left_around || isTRUE(mines_left < mines_left_around)) {
      return(
        list(
          clicks = "impossible",
          global_cache = global_cache,
          clusters_cache = clusters_cache
        )
      )
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
        clusters_cache = clusters_cache,
        click_order = click_order,
        to_clusterise = FALSE,
        in_cluster = in_cluster,
        ...
      )
      for (k in which(!is.na(possible))) {
        in_cache_yet <- is_in_global_cache(
          global_cache, 
          position_to_i_mat(
            pos_unknown[combins[, !mines_has_mine_i, drop = FALSE][, k], , drop = FALSE],
            dims
          )
        )
        if (!in_cache_yet) {
          global_cache[[length(global_cache) + 1]] <- list(
            position_to_i_mat(
              pos_unknown[
                combins[, !mines_has_mine_i, drop = FALSE][, k],
                ,
                drop = FALSE
              ],
              dims
            ),
            possible[k]
          )
        }
      }
      possible <- possible[!is.na(possible)]
      cache[which(!mines_has_mine_i)[seq_along(possible)]] <- possible
      if (hypothesis != 2 && all(!possible)) return(list(
        clicks = list(list(pos_unknown[mine_i, ], FALSE, "certain")),
        global_cache = global_cache,
        clusters_cache = clusters_cache
      ))
      if (any(possible)) {
        impossible <- FALSE
      }
      if (hypothesis == 2 && !impossible) {
        # on a trouvé un cas possible, on va dire que le clicker ne sait pu quoi faire pour l'instant
        # fonctionne uniquement en mode hypothesis, et parce que certain_core va retourner NULL, et que dans
        # main_game_loop, va trouver une exception "le clicker ne sait pu quoi faire" et renvoyer ça à
        # is_mine_propagation_possible qui va dire TRUE
        return(list(clicks = NULL, global_cache = global_cache, clusters_cache = clusters_cache))
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
        clusters_cache = clusters_cache,
        click_order = click_order,
        to_clusterise = FALSE,
        in_cluster = in_cluster,
        ...
      )
      for (k in which(!is.na(possible))) {
        in_cache_yet <- is_in_global_cache(
          global_cache, 
          position_to_i_mat(
            pos_unknown[combins[, mines_has_mine_i, drop = FALSE][, k], , drop = FALSE],
            dims
          )
        )
        if (!in_cache_yet) {
          global_cache[[length(global_cache) + 1]] <- list(
            position_to_i_mat(
              pos_unknown[
                combins[, mines_has_mine_i, drop = FALSE][, k],
                ,
                drop = FALSE
              ],
              dims
            ),
            possible[k]
          )
        }
      }
      possible <- possible[!is.na(possible)]
      cache[which(mines_has_mine_i)[seq_along(possible)]] <- possible
      if (hypothesis != 2 && all(!possible)) return(list(
        clicks = list(list(pos_unknown[mine_i, ], TRUE, "certain")),
        global_cache = global_cache,
        clusters_cache = clusters_cache
      ))
      if (any(possible)) {
        impossible <- FALSE
      }
      if (hypothesis == 2 && !impossible) {
        return(list(clicks = NULL, global_cache = global_cache, clusters_cache = clusters_cache)) # voir commentaire précédent
      }
      if (hypothesis == 2 && isTRUE(all(!cache))) return(list(
        clicks = "impossible",
        global_cache = global_cache,
        clusters_cache = clusters_cache
      ))
    }
  }

  tmp <- deduce_unknown_boxes(grid_init, mines_left_init, in_cluster, clusters_cache)
  if (!is.null(tmp)) return(list(clicks = tmp, global_cache = global_cache, clusters_cache = clusters_cache))
  if (isTRUE(all.equal(tmp, "impossible"))) return(list(clicks = "impossible", global_cache = global_cache, clusters_cache = clusters_cache))
  if (hypothesis != 2 && reached_prop && impossible) browser() # pas sensé etre impossible si on n'est pas en exploration
  if (hypothesis == 2 && reached_prop) return(list(clicks = "impossible", global_cache = global_cache, clusters_cache = clusters_cache))
  list(clicks = NULL, global_cache = global_cache, clusters_cache = clusters_cache) # ne sait pas quoi faire
}

is_in_global_cache <- function(global_cache, idx) {
  if (length(global_cache) == 0) return(FALSE)
  any(sapply(
    global_cache,
    function(cache_lst) {
      length(
        fast_setdiff_no_unique(
          cache_lst[[1]],
          idx
        )
      ) == 0 &
        length(
          fast_setdiff_no_unique(
            idx,
            cache_lst[[1]]
          )
        ) == 0 
    }
  ))
}

is_super_set_in_global_cache <- function(global_cache, idx) {
  sapply(
    global_cache,
    function(cache_lst) {
      if (cache_lst[[2]]) {
        return(all(idx %in% cache_lst[[1]]))
      } else {
        return(all(cache_lst[[1]] %in% idx))
      }
    }
  )
}


deduce_unknown_boxes <- function(grid, mines_left, in_cluster, clusters_cache) {
  if (is.na(mines_left)) return(NULL)
  known_boxes <- grid %in% known

  if (!is.null(in_cluster)) {
    known_boxes <- known_boxes | !in_cluster
    no_info_boxes <- sum(!known_boxes)
    if (no_info_boxes < mines_left) return("impossible")
    if (no_info_boxes == mines_left) return(NULL) # ne sait simplement plus quoi cliquer dans les autres clusters
  }
  
  dims <- dim(grid)
  if (mines_left < 0) return("impossible")
  if (mines_left == 0) return(lapply(which(!grid %in% known), function(i) list(i_to_position(i, dims), TRUE, "certain")))

  no_info_boxes <- sum(!known_boxes)
  if (no_info_boxes < mines_left) return("impossible")
  if (no_info_boxes == mines_left) return(list(list(i_to_position(which(!known_boxes)[1], dims), FALSE, "certain")))

  solved_around <- init_solved_around(grid)
  # on a aucune information sur les boîtes
  if (all(solved_around != 0) && no_info_boxes > mines_left) {
    return(NULL)
  }
  
  res <- is_void_solvable(grid, solved_around, mines_left,
                          clusters = clusters_cache %||% independant_clusters(grid, solved_around, mines_left))
  if (!isFALSE(res)) {
    return(res)
  }
  
  NULL
}

is_void_solvable <- function(grid, solved_around, mines_left, clusters) {
  res <- are_no_mine_in_void(grid, solved_around, mines_left, clusters)
  if (!isFALSE(res)) {
    return(res)
  }
  is_void_full_mines(grid, solved_around, mines_left, clusters)
}

are_no_mine_in_void <- function(grid, solved_around, mines_left, clusters) {
  known_boxes <- grid %in% known
  
  # petit raccourci : s'il y a moins de mines que le nombre de boîtes à découvrir
  if (mines_left <= sum(solved_around == 0 & !known_boxes)) {
    dims <- dim(grid)
    if (length(clusters$clusters) == 0 && mines_left == 0) {
      next_i <- which(clusters$void$in_cluster)[1]
      return(list(list(i_to_position(next_i, dims), TRUE, "certain")))
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
    if (isTRUE(sum(clusters_bornes_min_precises) == mines_left)) {
      return(
        lapply(which(clusters$void$in_cluster), function(next_i) {
          list(i_to_position(next_i, dims), TRUE, "certain")
        })
      )
    }
  }
  FALSE
}

is_void_full_mines <- function(grid, solved_around, mines_left, clusters) {
  if (mines_left == 0) return(FALSE)
  n_box_in_void <- sum(clusters$void$in_cluster)
  if (n_box_in_void > mines_left) return(FALSE)
  if (n_box_in_void == 0) return(FALSE)
  
  clusters_bornes_max_precises <- precise_clusters_bounds_max_shortcut_full_void(grid, solved_around, mines_left, clusters)
  remaining_for_void <- mines_left - sum(clusters_bornes_max_precises)

  dims <- dim(grid)
  if (isTRUE(n_box_in_void == remaining_for_void)) {
    return(
      lapply(which(clusters$void$in_cluster), function(next_i) {
        list(i_to_position(next_i, dims), FALSE, "certain")
      })
    )
  }
  FALSE
}

precise_clusters_bounds_min_shortcut <- function(grid, solved_around, mines_left, clusters) {
  # tester tout de suite avec mines_left
  if (length(clusters$clusters) == 1) {
    lst <- clusters$clusters[[1]]
    bornes_mines <- lst$bornes_mines
    # si ce n'est pas possible, on sait que le shortcut dans deduce_unknown_boxes ne déclanchera pas
    if (lst$possible) return(NULL)
    possibility <- test_trial(grid, mines_left, lst$in_cluster, mines_left)
    if (possibility) {
      # vérifier si c'est bel et bien la bornes min à mines_left, si oui, on pognera le shortcut dans deduce_unknown_boxes
      trial <- mines_left - 1
      while (trial >= 0 && trial >= bornes_mines[1]) {
        if (lst$possible[left] == "TRUE") {
          possibility <- TRUE
        } else if (lst$possible[left] == "FALSE") {
          possibility <- FALSE
        } else {
          possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[left])
        }

        if (possibility) {
          # volontairement retourner un nombre erroné pour pas que le SHORTCUT dans are_no_mine_in_void déclenche
          return(NA)
        } else {
          trial <- trial - 1
        }
      }
      return(mines_left)
    } else {
      # volontairement retourner un nombre erroné pour pas que le SHORTCUT dans are_no_mine_in_void déclenche
      return(NA)
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
        if (lst$possible[left] == "TRUE") {
          possibility <- TRUE
        } else if (lst$possible[left] == "FALSE") {
          possibility <- FALSE
        } else {
          possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[left])
        }

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
      return(NA)
    }
  }
  res
}

precise_clusters_bounds_max_shortcut_full_void <- function(grid, solved_around, mines_left, clusters) {
  n_box_in_void <- sum(clusters$void$in_cluster)
  res <- numeric(length(clusters$clusters))
  cumul_max_shortcut <- 0
  for (i in seq_along(res)) {
    lst <- clusters$clusters[[i]]
    # cluster d'une seule case connue
    if (sum(lst$in_cluster) == 1) {
      res[i] <- 0
    } else {
      trials <- lst$bornes_mines[1]:lst$bornes_mines[2]
      left <- 1
      right <- length(trials)
      max_possible <- NA
      while (left <= right) {
        if (lst$possible[right] == "TRUE") {
          possibility <- TRUE
        } else if (lst$possible[right] == "FALSE") {
          possibility <- FALSE
        } else {
          possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[right])
        }

        if (possibility) {
          max_possible <- trials[right]
          break
        } else {
          right <- right - 1
        }
      }
      res[i] <- max_possible
    }
    
    cumul_max_shortcut <- cumul_max_shortcut + res[i]
    if ((mines_left - cumul_max_shortcut) < n_box_in_void) {
      # volontairement retourner un nombre erroné pour pas que le SHORTCUT dans is_void_full_mines déclenche
      return(NA)
    }
  }
  res
}

#' Retourne si une proposition de mines est possible (génère une partie sans problèmes)
which_combins_possible <- function(grid, combins, pos_unknown, solved_around, mines_left,
                                   cache = rep(NA, ncol(combins)), global_cache = list(),
                                   clusters_cache = NULL,
                                   click_order = NULL, ...) {
  # mettre à jour la cache car on va cliquer
  if (length(global_cache) > 0) {
    keep <- !sapply(global_cache, `[[`, 2)
    global_cache <- global_cache[keep]
  }

  dims <- dim(grid)
  mines_left_init <- mines_left
  solved_around_init <- solved_around
  global_cache_init <- global_cache
  clusters_cache_init <- clusters_cache
  possible <- rep(NA, ncol(combins))
  for (i in seq_len(ncol(combins))) {
    mines_left <- mines_left_init
    solved_around <- solved_around_init
    global_cache <- global_cache_init
    clusters_cache <- clusters_cache_init
    if (!is.na(cache[i])) {
      possible[i] <- cache[i]
      next
    }
    combin <- combins[, i]
    find_in_global_cache <- if (length(global_cache) == 0) {
      NULL
    } else {
      which(is_super_set_in_global_cache(
        global_cache,
        position_to_i_mat(pos_unknown[combin, , drop = FALSE], dims)
      ))
    }
    if (!is.null(find_in_global_cache) && any(find_in_global_cache)) {
      possible[i] <- all(sapply(global_cache[find_in_global_cache], function(x) x[[2]]))
      next
    }
    # supposer des mines
    # puis propager avec certitude, et voir si c'est possible
    grid_tmp_propagate <- convert_grid_solution_to_human_grid(grid, solved_around, ...)
    i_to_flag <- position_to_i_mat(pos_unknown[combin, , drop = FALSE], dims)
    i_to_click <- position_to_i_mat(pos_unknown[-combin, , drop = FALSE], dims)
    i_to_click <- i_to_click[grid[i_to_click] %in% c(unknown_box, hp_to_hypo_no_mine)]

    for (j in i_to_flag) {
      tmp <- apply_action(grid_tmp_propagate, i_to_position(j, dims), action = FALSE, mines_left,
                          solved_around, hypothesis = 2, ...)
      grid_tmp_propagate <- tmp[[1]]
      mines_left <- tmp[[3]]
      solved_around <- tmp[[4]]
    }

    for (j in i_to_click) {
      j_pos <- i_to_position(j, dims)
      tmp <- apply_action(grid_tmp_propagate, j_pos, action = TRUE, mines_left,
                          solved_around, hypothesis = 2, ...)
      grid_tmp_propagate <- tmp[[1]]
      mines_left <- tmp[[3]]
      solved_around <- tmp[[4]]
      click_order <- rbind(click_order, j_pos)
    }
    
    # mettre à jour la cache
    global_cache <- update_global_cache(global_cache, grid, grid_tmp_propagate)
    clusters_cache_args <- list(
      clusters_cache = clusters_cache, grid = grid, new_grid = grid_tmp_propagate,
      solved_around = solved_around, mines_left = mines_left
    )

    possible[i] <- is_mine_propagation_possible(grid_tmp_propagate, mines_left = mines_left,
                                                solved_around = solved_around, click_order = click_order, 
                                                global_cache = global_cache,
                                                clusters_cache_args = clusters_cache_args, ...)
    if (possible[i]) break # early exist cause the calling function (which_combins_possible) checks for all FALSE
  }
  possible
}

convert_grid_solution_to_human_grid <- function(grid, solved_around = init_solved_around(grid), ...) {
  human_grid <- grid
  human_grid[solved_around == -1 & !grid %in% known] <- unknown_box
  human_grid[human_grid %in% hp_to_hypo_no_mine] <- unknown_box
  human_grid
}
