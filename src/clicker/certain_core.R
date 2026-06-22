source("src/fast_apply.R")
source("src/indicies/count.R")
source("src/game_engine/is_grid_possible.R")
source("src/game_engine/convert_grid_solution_to_human_grid.R")
source("src/clicker/helper/update_cache.R")
source("src/clicker/helper/deduce_unknown_boxes.R")
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

can_deduce_pattern <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL,
                               global_cache = list(), clusters_cache = NULL, ...) {
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
      tmp_which_possible <- which_combins_possible(
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
        ...
      )
      possible <- tmp_which_possible$possible
      clusters_cache <- tmp_which_possible$clusters_cache
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
      tmp_which_possible <- which_combins_possible(
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
        ...
      )
      possible <- tmp_which_possible$possible
      clusters_cache <- tmp_which_possible$clusters_cache
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

  tmp <- deduce_unknown_boxes(grid_init, mines_left_init, clusters_cache, ...)
  if (!is.null(tmp$clicks)) return(list(clicks = tmp$clicks, global_cache = global_cache, clusters_cache = tmp$clusters_cache))
  if (isTRUE(all.equal(tmp$clicks, "impossible"))) return(list(clicks = "impossible", global_cache = global_cache, clusters_cache = tmp$clusters_cache))
  if (hypothesis != 2 && reached_prop && impossible) browser() # pas sensé etre impossible si on n'est pas en exploration
  if (hypothesis == 2 && reached_prop) return(list(clicks = "impossible", global_cache = global_cache, clusters_cache = tmp$clusters_cache))
  list(clicks = NULL, global_cache = global_cache, clusters_cache = tmp$clusters_cache) # ne sait pas quoi faire
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
    tmp_possible <- is_mine_propagation_possible(grid_tmp_propagate, mines_left = mines_left,
                                                 solved_around = solved_around, click_order = click_order, 
                                                 global_cache = global_cache, clusters_cache = clusters_cache_init, ...)
    possible[i] <- tmp_possible$possible

    # conserver la cache des clusters indépendants qui n'étaient pas affectés par les mines temporaires posées
    if (is.null(tmp_possible$clusters_cache)) {
      clusters_cache <- NULL
    } else {
      if (is.null(clusters_cache_init)) {
        clusters_cache_init <- independant_clusters(grid, solved_around_init, mines_left_init)
      } 
      cache_kept <- mapply(
        function(clust_a, clust_b) all(clust_a$grid == clust_b$grid), # aucun changement n'était survenu dans ces clusts
        clusters_cache_init$clusters,
        tmp_possible$clusters_cache$clusters
      )
      if (is.list(cache_kept)) browser()
      clusters_cache$clusters[cache_kept] <- tmp_possible$clusters_cache$clusters[cache_kept]
    }

    if (possible[i]) break # early exist cause the calling function (which_combins_possible) checks for all FALSE
  }

  list(possible = possible, clusters_cache = clusters_cache)
}
