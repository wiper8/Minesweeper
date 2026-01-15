source("src/indicies.R")

update_grid_status <- function(grid, human = FALSE) {
  for (i in which(grid == uncovered_no_mine)) {
    pos <- i_to_position(i, dim(grid))
    square <- get_around_square(pos, grid)
    grid[i] <- compute_box_number(square, human) # calculer le chiffre à mettre
    
    if (grid[i] == 0) {
      # cliquer à nouveau automatiquement tout autour
      positions <- square_pos(pos, grid)
      reveal <- unlist(square) == covered_no_mine
      positions <- positions[reveal, , drop = FALSE]
      for (j in seq_len(nrow(positions))) {
        grid[positions[j, 1], positions[j, 2]] <- uncovered_no_mine
      }
      grid <- update_grid_status(grid)
    }
  }
  grid
}

compute_box_number <- function(square, human = FALSE) {
  sum(square %in% c(covered_mine, flag_on_mine) | (square == flag_on_no_mine & human))
}

is_grid_possible <- function(grid) {
  
  for (i in which(grid >= 0)) {
    pos <- i_to_position(i, dim(grid))
    square <- get_around_square(pos, grid)
    if (grid[i] > sum(square %in% c(covered_no_mine, covered_mine, flag_on_mine, flag_on_no_mine))) return(FALSE)
    if (grid[i] < sum(square %in% c(flag_on_mine, flag_on_no_mine))) return(FALSE)
  }
  TRUE
}

sanity_check <- function(grid, status = NULL) {
  if (is.null(status)) {
    status <- matrix(1, nrow(grid), ncol(grid))
  }
  
  for (next_i in which(grid > 0 & status)) {
    pos <- i_to_position(next_i, dim(grid))
    values <- get_around_square(pos, grid)
    unclicked <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine, uncovered_unknown)
    unknown <- unclicked & values != flag_on_mine
    n_unknown <- sum(unknown)
    if (n_unknown == 0) next
    mines_left <- (grid[next_i] - sum(values %in% c(flag_on_mine, flag_on_no_mine)))
    if (mines_left == 0) next
    if (mines_left < 0) {
     return(FALSE)
    }
  }
  TRUE
}


