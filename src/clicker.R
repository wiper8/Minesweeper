source("src/indicies.R")
source("src/game_engine.R")

random_first_click <- function(dims) {
  c(sample(1:dims[1], 1), sample(1:dims[2], 1))
}

smart_first_click <- function(dims) {
  # TODO
}

random_clicker <- function(grid, mines, ...) {
  if (sum(grid %in% c(covered_no_mine, covered_mine)) == 0) return(list(grid, NULL, "no box left", NULL))
  next_i <- sample(which(grid %in% c(covered_no_mine, covered_mine)), 1)
  list(grid, next_i, "random", matrix(0, nrow(grid), ncol(grid)))
}

certain_else_random_clicker <- function(grid, mines, status = NULL) {
  tmp <- certain_core(grid, status = status)
  if (tmp[[3]] == "certain") {
    return(tmp)
  }
  grid <- tmp[[1]]
  # si aucune stratégie
  random_clicker(grid, mines)
}

human_clicker <- function(grid, mines, status = NULL) {
  grid_init <- grid
  tmp <- certain_core(grid, status = status)
  if (tmp[[3]] == "certain") {
    return(tmp)
  }
  grid <- tmp[[1]]
  status <- tmp[[4]]
  remaining_mines <- mines - sum(grid %in% c(flag_on_mine, flag_on_no_mine))
  adjacent_to_uncovered_boxes <- sapply(
    which(grid > 0 & status == 1),
    function(i_uncovered) {
      pos <- i_to_position(i_uncovered, dim(grid))
      positions <- square_pos(pos, grid)
      
      values <- get_around_square(pos, grid)
      unclicked <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine, uncovered_unknown)
      unknown <- unclicked & values != flag_on_mine
      apply(positions[unknown, , drop = FALSE], 1, function(x) position_to_i(x, dim(grid)))
    }
  ) |> unlist() |> unique() |> as.vector()
  if (length(adjacent_to_uncovered_boxes) == 0) return(random_clicker(grid, mines))
  # probabilité estimé si je guess sur une case adjacente à un nombre
  prob_mine_approx <- sapply(adjacent_to_uncovered_boxes, function(i) {
    pos <- i_to_position(i, dim(grid))
    positions <- square_pos(i_to_position(i, dim(grid)), grid)
    values <- get_around_square(pos, grid)
    known_number_adjacent <- positions[values >= 0, , drop = FALSE]
    values <- values[values >= 0]
    mean(apply(known_number_adjacent, 1, function(pos) {
      values_around_number <- get_around_square(pos, grid)
      mines_around <- sum(values_around_number %in% c(flag_on_mine, flag_on_no_mine))
      
      unclicked <- !values_around_number %in% c(0:9, flag_on_mine, flag_on_no_mine, uncovered_unknown)
      unknown_around <- sum(unclicked & values_around_number != flag_on_mine)
      (grid[pos[1], pos[2]] - mines_around) / unknown_around
    }))
  })
  
  unclicked <- which(grid %in% c(covered_no_mine, covered_mine))
  unclicked_nor_adja <- setdiff(unclicked, adjacent_to_uncovered_boxes)
  if (length(unclicked_nor_adja) == 0) {
    prob_in_random_middle_squares <- 0.99
  } else {
    prob_in_random_middle_squares <- (mines - sum(grid %in% c(flag_on_mine, flag_on_no_mine))) / length(unclicked)
  }
  if (min(prob_mine_approx) > prob_in_random_middle_squares) {
    next_i <- sample(unclicked_nor_adja, 1)
  } else {
    next_i <- adjacent_to_uncovered_boxes[which.min(prob_mine_approx)]
  }
  list(grid, next_i, "probabilistic", grid_init != grid)
}

smart_clicker <- function(grid, mines, status = NULL) {
  grid_init <- grid
  tmp <- certain_core(grid, status = status)
  if (tmp[[3]] == "certain") {
    return(tmp)
  }
  grid <- tmp[[1]]
  status <- tmp[[4]]
  remaining_mines <- mines - sum(grid %in% c(flag_on_mine, flag_on_no_mine))
  adjacent_to_uncovered_boxes <- sapply(
    which(grid > 0 & status == 1),
    function(i_uncovered) {
      positions <- square_pos(i_to_position(i_uncovered, dim(grid)), grid)
      
      values <- apply(positions, 1, function(ids) grid[ids[1], ids[2]])
      unclicked <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine, uncovered_unknown)
      unknown <- unclicked & values != flag_on_mine
      apply(positions[unknown, , drop = FALSE], 1, function(x) position_to_i(x, dims))
    }
  ) |> unlist() |> unique() |> as.vector()
  
  non_adjacent_to_uncovered_boxes <- sapply(
    setdiff(seq_len(length(grid)), which(grid > 0)),
    function(i_uncovered) {
      pos <- i_to_position(i_uncovered, dim(grid))
      positions <- square_pos(pos, grid)
      
      values <- get_around_square(pos, grid) # apply(positions, 1, function(ids) grid[ids[1], ids[2]])
      unclicked <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine, uncovered_unknown)
      unknown <- unclicked & values != flag_on_mine
      apply(positions[unknown, , drop = FALSE], 1, function(x) position_to_i(x, dims))
    }
  ) |> unlist() |> unique() |> as.vector()
  
  sum(non_adjacent_to_uncovered_boxes)
  # TODO
}

certain_core <- function(grid, status) {
  
  # tous les cas où le nombre de cases inconnues autour, non flaguées = chiffre
  tmp <- mines_left_equals_boxes_left(grid, status)
  if (tmp[[3]] == "certain") {
    return(tmp)
  }
  grid <- tmp[[1]]
  status <- tmp[[4]]
  
  tmp <- certain_flag_or_nomine_pattern(grid, status)
  if (tmp[[3]] %in% c("certain", "impossible"))
  if (tmp[[3]] == "certain") {
    return(tmp)
  }
  status <- tmp[[4]]
  list(grid, NULL, "ne sais pas quoi cliquer", status)
}

mines_left_equals_boxes_left <- function(grid, status = NULL) {
  grid_init <- grid
  # tous les cas où le nombre de cases inconnues autour, non flaguées = chiffre
  if (is.null(status)) {
    status <- matrix(1, nrow(grid), ncol(grid))
  }
  
  for (next_i in which(grid > 0 & status)) {
    pos <- i_to_position(next_i, dim(grid))
    positions <- square_pos(pos, grid)
    
    values <- get_around_square(pos, grid)
    unclicked <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine, uncovered_unknown)
    unknown <- unclicked & values != flag_on_mine
    n_unknown <- sum(unknown)
    mines_left <- (grid[next_i] - sum(values %in% c(flag_on_mine, flag_on_no_mine)))
    # il reste 0 mines potentielles autour
    if (mines_left == 0 && n_unknown > 0) {
      return(list(grid, position_to_i(unlist(positions[unknown, , drop = FALSE][1, ]), dim(grid)), "certain", grid_init != grid))
    } else {
      # ou il reste autant de mines que de cases potentielles autour
      flag <- (n_unknown == mines_left) && n_unknown > 0
      if (flag) {
        j <- apply(positions[unclicked, , drop = FALSE], 1, function(pos) position_to_i(pos, dim(grid)))
        grid <- flagguer(grid, j)
        status[j] <- 1
        status <- status_around(status)
      }
    }
  }
  status <- status_around(grid_init != grid) | status
  list(grid, NULL, "ne sait pas quoi cliquer", status)
}

certain_flag_or_nomine_pattern <- function(grid, status = NULL) {
  grid_init <- grid
  if (is.null(status)) {
    status <- matrix(1, nrow(grid), ncol(grid))
  }
  if (!is_grid_possible(grid)) return(list(grid, NULL, "impossible", status))
  for (next_i in which(grid > 0 & status)) {
    pos <- i_to_position(next_i, dim(grid))
    positions <- square_pos(pos, grid)
    
    values <- get_around_square(pos, grid)
    values_pos <- square_pos(pos, grid)
    unclicked <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine, uncovered_unknown)
    unknown <- unclicked & values != flag_on_mine
    n_unknown <- sum(unknown)
    if (n_unknown == 0) next
    mines_left <- grid[next_i] - sum(values %in% c(flag_on_mine, flag_on_no_mine))
    if (mines_left == 0) next
    if (mines_left < 0) return(list(grid, NULL, "impossible", status))
    
    # appliquer toutes les combins de mines autour, et vérifier s'il y a une certitude
    pos_unknown <- values_pos[unknown, , drop = FALSE]
    
    # tester toutes les combinaisons autour de cette case, vérifier s'il y a toujours ou jamais un drapeau
    combins <- combn(n_unknown, mines_left)
    possible <- rep(NA, ncol(combins))
    for (mine_j in seq_len(ncol(combins))) {
      grid_tmp_propagate <- grid
      combin <- combins[, mine_j]
      i_to_flag <- position_to_i_mat(pos_unknown[combin, , drop = FALSE], dim(grid))
      status[i_to_flag] <- 1
      status <- status_around(status)
      
      grid_tmp_propagate[i_to_flag] <- ifelse(grid_tmp_propagate[i_to_flag] == covered_mine, flag_on_mine, flag_on_no_mine) # supposer des mines
      possible[mine_j] <- is_game_possible(grid_tmp_propagate, status)
    }
    for (mine_i in seq_len(n_unknown)) {
      if (all(!possible)) stop("erreur, grille impossible?")
      # certain qu'il n'y ait pas de mine : donc cliquer
      possible_combins <- combins[, possible, drop = FALSE]
      mines_has_mine_i <- apply(possible_combins, 2, `%in%`, x = mine_i)
      if (all(!mines_has_mine_i)) {
        i_to_flag <- position_to_i(pos_unknown[mine_i, ], dim(grid))
        return(list(grid, i_to_flag, "certain", grid_init != grid | seq_len(length(grid)) == i_to_flag))
      }
      # impossible qu'il y ait une mine : donc flagger
      if (all(mines_has_mine_i)) {
        i_to_flag <- position_to_i(pos_unknown[mine_i, ], dim(grid))
        grid <- flagguer(grid, i_to_flag)
        status[i_to_flag] <- 1
        status <- status_around(status)
        
        return(certain_core(grid, status)) # récursif
      }
    }
  }
  list(grid, NULL, "impossible", status)
}

flagguer <- function(grid, i) {
  grid[i] <- ifelse(grid[i] == covered_mine, flag_on_mine, flag_on_no_mine)
  grid
}


is_game_possible <- function(grid, status = NULL) {
  if (is.null(status)) {
    status <- matrix(1, nrow(grid), ncol(grid))
  }
  
  repeat {
    # propagate boxes numbers
    grid2 <- update_grid(grid, human = TRUE)
    status[status_around(grid2 != grid) & status == 0] <- 1
    grid <- grid2
    
    # success
    if (sum(grid %in% c(covered_no_mine, uncovered_no_mine, flag_on_no_mine)) == 0 ||
        sum(grid %in% c(covered_no_mine, flag_on_no_mine)) == 0) {
      return(TRUE)
    }
    # next box to try
    if (!sanity_check(grid, status)) return(FALSE)
    tmp <- certain_core(grid, status)
    if (tmp[[3]] == "impossible") return(FALSE)
    
    grid <- tmp[[1]] # flags added
    next_click <- tmp[[2]]
    status <- tmp[[4]]
    # uncover box
    if (!is.null(next_click)) {
      pos <- i_to_position(next_click, dim(grid))
      grid[next_click] <- uncovered_unknown
      status[next_click] <- 1
      status <- status_around(status)
      
      if (!is_grid_possible(grid)) return(FALSE)
    }
  }
}

status_around <- function(status) {
  for (i in which(status == 1)) {
    pos <- i_to_position(i, dim(status))
    positions <- square_pos(pos, status)
    for (j in seq_len(nrow(positions))) {
      status[positions[j, 1], positions[j, 2]] <- 1
    }
  }
  status
}

