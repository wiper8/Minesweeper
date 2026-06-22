source("src/game_engine/init_solved_around.R")
source("src/game_engine/convert_grid_solution_to_human_grid.R")
source("src/clicker/helper/is_mine_propagation_possible.R")

test_trial <- function(grid, mines_left, in_cluster, trial) {
  # préciser les bornes
  tmp_grid <- grid
  # pour simplifier, on met des no-mines partout ailleurs
  tmp_grid[in_cluster == 0] <- void_box
  new_solved_around <- init_solved_around(tmp_grid)
  
  # résoudre le cluster avec `trial` mines
  res <- is_mine_propagation_possible(
    convert_grid_solution_to_human_grid(tmp_grid, new_solved_around),
    trial,
    new_solved_around,
    to_clusterise = FALSE,
    in_cluster = in_cluster
  )
  if (!res$possible) return(FALSE)
  
  # résoudre le reste sans le cluster avec mines_left - trial mines
  tmp_grid <- grid
  tmp_grid[in_cluster == 1 & !grid %in% known] <- void_box # & !grid %in% known car sinon on pourrait masquer des cellules
  # essentielles aux autres clusters
  new_solved_around <- init_solved_around(tmp_grid)
  
  is_mine_propagation_possible(
    convert_grid_solution_to_human_grid(tmp_grid, new_solved_around),
    mines_left - trial,
    new_solved_around,
    to_clusterise = FALSE
  )$possible
}
