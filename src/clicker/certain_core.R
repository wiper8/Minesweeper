source("src/fast_apply.R")
source("src/indicies/count.R")
source("src/game_engine/is_grid_possible.R")
source("src/clicker/is_mine_propagation_possible.R")

certain_core <- function(grid, total_mines, solved_around, hypothesis, ...) {
  if (hypothesis && tryCatch(
    is_game_over(grid, total_mines),
    error = function(e) {
      "nombre invalide de mines"
    }
  ) == "nombre invalide de mines") {
    return("impossible")
  }
  tmp <- can_flag_all_around(grid, solved_around)
  if (!is.null(tmp)) return(tmp)
  tmp <- can_click_all_around(grid, solved_around)
  if (!is.null(tmp)) return(tmp)
  tmp <- can_deduce_pattern(grid, total_mines, solved_around, hypothesis)
  if (!is.null(tmp)) return(tmp)
  NULL # retourner NULL si on ne sait pas quelle action certain prendre.
}

can_flag_all_around <- function(grid, solved_around) {
  for (i in which(grid > 0 & solved_around == 0)) {
    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    n_unknown <- count_unknown(grid, i, values)
    if (n_unknown > 0 && count_mines_left_around(grid, i, values) == n_unknown) {
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

can_deduce_pattern <- function(grid, total_mines, solved_around, hypothesis = FALSE) {
  impossible <- TRUE
  
  for (i in which(grid > 0 & solved_around == 0)) {
    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    unknown <- !values %in% known
    n_unknown <- sum(unknown)
    
    # car quand on essaie un drapeau et de le propager, ça peut arriver qu'il n'y a plus de combinaisons
    if (n_unknown == 0) next
    
    mines_left <- count_mines_left_around(grid, i, values)
    # appliquer toutes les combins de mines autour, et vérifier s'il y a une certitude
    pos_unknown <- positions[unknown, , drop = FALSE]
    
    # tester toutes les combinaisons autour de cette case, vérifier s'il y a toujours ou jamais un drapeau
    # dans les situations où on propage un flag, ça peut arriver
    if (mines_left < 0 | n_unknown < mines_left) return("impossible")
    
    combins <- combn(n_unknown, mines_left)
    
    for (mine_i in seq_len(n_unknown)) {
      mines_has_mine_i <- fast_apply(combins, 2, function(comb) mine_i %in% comb)
      
      # impossible qu'il y ait pas de mine : donc flagger
      possible <- which_combins_possible(
        grid,
        combins[, !mines_has_mine_i, drop = FALSE],
        pos_unknown,
        solved_around = solved_around,
        total_mines = total_mines
      )
      if (!hypothesis && all(!possible)) return(list(pos_unknown[mine_i, ], FALSE))
      if (any(possible)) impossible <- FALSE
      
      # impossible qu'il y ait une mine : donc cliquer
      possible <- which_combins_possible(
        grid,
        combins[, mines_has_mine_i, drop = FALSE],
        pos_unknown,
        solved_around = solved_around,
        total_mines = total_mines
      )
      if (!hypothesis && all(!possible)) return(list(pos_unknown[mine_i, ], TRUE))
      if (any(possible)) impossible <- FALSE
    }
  }
  if (impossible) return("impossible")
  NULL
}

#' Retourne si une proposition de mines est possible (génère une partie sans problèmes)
which_combins_possible <- function(grid, combins, pos_unknown, ...) {
  possible <- rep(NA, ncol(combins))
  for (i in seq_len(ncol(combins))) {
    combin <- combins[, i]
    # supposer des mines
    # puis propager avec certitude, et voir si c'est possible
    grid_tmp_propagate <- convert_grid_solution_to_human_grid(grid, ...)
    i_to_flag <- position_to_i_mat(pos_unknown[combin, , drop = FALSE], dim(grid))
    i_to_click <- position_to_i_mat(pos_unknown[-combin, , drop = FALSE], dim(grid))
    i_to_click <- i_to_click[grid[i_to_click] %in% c(covered_mine, covered_no_mine, uncovered_mine, uncovered_no_mine)]
    grid_tmp_propagate[i_to_flag] <- hypothetical_mine
    grid_tmp_propagate[i_to_click] <- hypothetical_no_mine
    possible[i] <- is_mine_propagation_possible(grid_tmp_propagate, ...)
    if (possible[i]) break # early exist cause the calling function checks for all FALSE
  }
  possible
}

convert_grid_solution_to_human_grid <- function(grid, solved_around, ...) {
  human_grid <- grid
  human_grid[solved_around == -1] <- unknown_box
  human_grid[human_grid %in% c(covered_mine, covered_no_mine, uncovered_mine, uncovered_no_mine)] <- unknown_box
  human_grid
}
