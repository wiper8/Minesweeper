source("src/fast_apply.R")
source("src/indicies/count.R")
source("src/game_engine/is_grid_possible.R")
source("src/clicker/is_mine_propagation_possible.R")

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
      if (isTRUE(mines_left == 0)) return("impossible")
      unknown <- !values %in% known
      return(list(positions[unknown, , drop = FALSE][1, ], FALSE))
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
      return(list(positions[unknown, , drop = FALSE][1, ], TRUE))
    }
  }
  NULL
}

can_deduce_pattern <- function(grid, mines_left, solved_around, hypothesis, click_order = NULL) {
  impossible <- TRUE # pour hypothesis = TRUE
  reached_prop <- FALSE
  if (is.null(click_order)) {
    i_to_investigate <- which(grid > 0 & solved_around == 0)
  } else {
    # browser()
    i_to_investigate <- position_to_i_mat(click_order[rev(seq_len(nrow(click_order))), , drop = FALSE], dim(grid))
    # s'assurer de juste investiguer les cases pertinentes
    i_to_investigate <- i_to_investigate[grid[i_to_investigate] > 0 & solved_around[i_to_investigate] == 0]
    # au cas où on en oubli, quand des cases sont révélées automatiquement sans avoir été cliquées
    i_to_investigate <- union(i_to_investigate, which(grid > 0 & solved_around == 0))
  }

  # je prend une cellule avec un chiffre qui a >= 1 inconnu autour
  for (i in i_to_investigate) {
    print(paste0("i:", i))
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
    
    combins <- combn(n_unknown, mines_left_around)
    cache <- rep(NA, ncol(combins))

    for (mine_i in seq_len(n_unknown)) {
      print(paste0("mine_i:", mine_i))
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
        click_order = click_order
      )
      possible <- possible[!is.na(possible)]
      cache[which(!mines_has_mine_i)[seq_along(possible)]] <- possible
      if (!hypothesis && all(!possible)) return(list(pos_unknown[mine_i, ], FALSE))
      if (any(possible)) {
        impossible <- FALSE
      }
      if (hypothesis && !impossible) {
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
        click_order = click_order
      )
      possible <- possible[!is.na(possible)]
      cache[which(mines_has_mine_i)[seq_along(possible)]] <- possible
      if (!hypothesis && all(!possible)) return(list(pos_unknown[mine_i, ], TRUE))
      if (any(possible)) {
        impossible <- FALSE
      }
      if (hypothesis && !impossible) {
        return(NULL) # voir commentaire précédent
      }
    }
  }
  tmp <- deduce_unknown_boxes(grid, mines_left)
  if (!is.null(tmp)) return(tmp)
  if (isTRUE(all.equal(tmp, "impossible"))) return("impossible")
  if (!hypothesis && impossible) browser() # pas sensé etre impossible si on n'est pas en exploration
  if (hypothesis && reached_prop) return("impossible")
  NULL # ne sait pas quoi faire
}

deduce_unknown_boxes <- function(grid, mines_left) {
  if (is.na(mines_left)) return(NULL)
  known_boxes <- grid %in% known
  if (mines_left < 0) return("impossible")
  if (mines_left == 0) return(list(i_to_position(which(!known_boxes)[1], dim(grid)), TRUE))
  no_info_boxes <- sum(!known_boxes)
  if (no_info_boxes < mines_left) return("impossible")
  if (no_info_boxes == mines_left) return(list(i_to_position(which(!known_boxes)[1], dim(grid)), FALSE))
  NULL
}

#' Retourne si une proposition de mines est possible (génère une partie sans problèmes)
which_combins_possible <- function(grid, combins, pos_unknown, mines_left, cache = rep(NA, ncol(combins)),
                                   click_order = NULL, ...) {
  browser()
  mines_left_init <- mines_left
  possible <- rep(NA, ncol(combins))
  for (i in seq_len(ncol(combins))) {
    if (!is.na(cache[i])) {
      possible[i] <- cache[i]
      next
    }
    combin <- combins[, i]
    # supposer des mines
    # puis propager avec certitude, et voir si c'est possible
    grid_tmp_propagate <- convert_grid_solution_to_human_grid(grid, ...)
    i_to_flag <- position_to_i_mat(pos_unknown[combin, , drop = FALSE], dim(grid))
    i_to_click <- position_to_i_mat(pos_unknown[-combin, , drop = FALSE], dim(grid))
    i_to_click <- i_to_click[grid[i_to_click] %in% hp_to_hypo_no_mine]
    mines_left <- mines_left_init - length(i_to_flag)
    grid_tmp_propagate[i_to_flag] <- hypothetical_mine
    grid_tmp_propagate[i_to_click] <- hypothetical_no_mine
    
    for (i_to_ck in i_to_click) {
      click_order <- rbind(click_order, i_to_position(i_to_ck, dim(grid)))
    }
    
    possible[i] <- is_mine_propagation_possible(grid_tmp_propagate, mines_left = mines_left, click_order = click_order, ...)
    if (possible[i]) break # early exist cause the calling function (which_combins_possible) checks for all FALSE
  }
  possible
}

convert_grid_solution_to_human_grid <- function(grid, solved_around, ...) {
  human_grid <- grid
  human_grid[solved_around == -1] <- unknown_box
  human_grid[human_grid %in% hp_to_hypo_no_mine] <- unknown_box
  human_grid
}
