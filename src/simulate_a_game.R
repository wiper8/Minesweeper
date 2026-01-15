source("src/hp.R")
source("src/clicker.R")
source("src/indicies.R")
source("src/game_engine.R")

simulate_a_game <- function(mines, dims = c(17, 9), clicker, verbose = FALSE) {
  verb <- function(grid, verbose) {
    if (verbose) {
      print(grid)
    }
  }
  
  grid <- matrix(NA, nrow = dims[1], ncol = dims[2])
  first_click <- random_first_click(dims) # TODO changer pour une meilleure fonction
  
  grid[first_click[1], first_click[2]] <- uncovered_no_mine
  # put mines in the game
  grid[sample(setdiff(seq_len(prod(dims)), position_to_i(first_click, dims)), mines)] <- covered_mine
  grid[is.na(grid)] <- covered_no_mine
  status <- matrix(0, nrow(grid), ncol(grid))
  
  repeat {
    # propagate boxes numbers
    {
      grid <- update_grid_status(grid)
      # success
      if (sum(grid %in% c(covered_no_mine, uncovered_no_mine, flag_on_no_mine)) == 0 ||
          sum(grid %in% c(covered_no_mine, flag_on_no_mine)) == 0) {
        verb(grid, verbose)
        return(TRUE)
      }
      # next box to try
      tmp <- clicker(grid, mines, status = status)
    }
    {
      grid <- tmp[[1]] # flags added
      next_click <- tmp[[2]]
      status <- tmp[[4]]
      
      # on perd même si on flag une mine pour simplifier le jeu
      if (any(grid == flag_on_no_mine)) {
        verb(grid, verbose)
        return(FALSE)
      }
      
      # uncover box
      if (!is.null(next_click)) {
        new_box_status <- grid[next_click]
        # failed
        if (new_box_status %in% c(covered_mine, flag_on_mine)) {
          grid[next_click] <- uncovered_mine
          verb(grid, verbose)
          return(FALSE)
        }
        grid[next_click] <- uncovered_no_mine
        status[next_click] <- 1
        status <- status_around(status)
      }
    }
  }
}
