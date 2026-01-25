source("src/indicies/count.R")
source("src/clicker/is_mine_propagation_possible.R")

certain_core <- function(grid, total_mines, solved_around) {
  tmp <- can_flag_all_around(grid, solved_around)
  if (!is.null(tmp)) return(tmp)
  tmp <- can_click_all_around(grid, solved_around)
  if (!is.null(tmp)) return(tmp)
  tmp <- can_deduce_pattern(grid, solved_around)
  if (!is.null(tmp)) return(tmp)
  # tmp <- can_deduce_pattern_knowing_mines_left()
  # if (!is.null(tmp)) return(tmp)
  
  NULL # retourner NULL si on ne sait pas quelle action certain prendre.
}

can_flag_all_around <- function(grid, solved_around) {
  for (i in which(grid > 0 & solved_around == 0)) {
    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    n_unknown <- count_unknown(grid, i, values)
    if (n_unknown > 0 && count_mines_left_around(grid, i, values) == n_unknown) {
      unknown <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine)
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
      unknown <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine)
      return(list(positions[unknown, , drop = FALSE][1, ], TRUE))
    }
  }
  NULL
}

can_deduce_pattern <- function(grid, solved_around) {
  for (i in which(grid > 0 & solved_around == 0)) {
    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    values_pos <- tmp$values_pos
    unknown <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine)
    n_unknown <- sum(unknown)
    
    # car quand on essaie un drapeau et de le propager, ça peut arriver qu'il n'y a plus de combinaisons
    if (n_unknown == 0) next
    
    mines_left <- count_mines_left_around(grid, i, values)
    # appliquer toutes les combins de mines autour, et vérifier s'il y a une certitude
    pos_unknown <- values_pos[unknown, , drop = FALSE]
    
    # tester toutes les combinaisons autour de cette case, vérifier s'il y a toujours ou jamais un drapeau
    combins <- combn(n_unknown, mines_left)
    possible <- which_combins_possible(grid, combins, pos_unknown, solved_around = solved_around)
    if (all(!possible)) browser()
    
    for (mine_i in seq_len(n_unknown)) {
      # certain qu'il n'y ait pas de mine : donc cliquer
      possible_combins <- combins[, possible, drop = FALSE]
      mines_has_mine_i <- apply(possible_combins, 2, `%in%`, x = mine_i) # TODO optimiser sans apply?
      if (all(!mines_has_mine_i)) return(list(pos_unknown[mine_i, ], TRUE))
      
      # impossible qu'il y ait pas de mine : donc flagger
      if (all(mines_has_mine_i)) return(list(pos_unknown[mine_i, ], FALSE))
    }
  }
  NULL
}

which_combins_possible <- function(grid, combins, pos_unknown, ...) {
  apply(combins, 2, function(combin) {
    # supposer des mines
    # puis propager avec certitude, et voir si c'est possible
    grid_tmp_propagate <- grid
    i_to_flag <- position_to_i_mat(pos_unknown[combin, , drop = FALSE], dim(grid))
    grid_tmp_propagate[i_to_flag] <- flag_on_mine
    is_mine_propagation_possible(grid_tmp_propagate, ...)
  })
}

