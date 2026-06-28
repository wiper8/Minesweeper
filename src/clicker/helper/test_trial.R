source("src/game_engine/init_solved_around.R")
source("src/game_engine/convert_grid_solution_to_human_grid.R")
source("src/clicker/helper/is_mine_propagation_possible.R")

test_trial <- function(grid, mines_left, in_cluster, trial) {
  res <- test_trial_shortcut(grid, in_cluster, trial)
  if (!is.null(res)) return(res)

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
  )$possible
  if (!res) return(FALSE)
  
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

test_trial_shortcut <- function(grid, in_cluster, trial) {
  counted <- grid * 0
  boxes_unknown <- sum(!grid %in% known & in_cluster)

  empty_boxes_count <- 0
  for (cand in which(grid >= 0 & in_cluster)) {
    tmp <- count_core(grid, cand)
    values <- tmp$values
    n_unknown <- count_unknown(grid, cand, values)
    if (n_unknown == 0) next

    # minimum à cause du <- 0 plus loin. Bref, le minimum permet de forcer n_unknown = n_mines_around
    n_mines_around <- min(n_unknown, count_mines_left_around(grid, cand, values))
    empty_boxes_count <- empty_boxes_count + n_unknown - n_mines_around

    # si on sait déjà que trial contient trop de mines
    if ((boxes_unknown - empty_boxes_count) < trial) return(FALSE)
    counted[position_to_i_mat(tmp$positions, dim(grid))] <- 1
  
    # faire comme si on connaissait ces boites, pour ne pas les recompter
    grid[counted == 1 & !grid %in% known] <- 0
  }
  NULL
}

