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
  grid_max <- grid
  # grid_min <- grid
  dims <- dim(grid)

  empty_boxes_count <- 0
  full_boxes_count <- 0
  for (cand in which(grid >= 0 & in_cluster)) {
    tmp_max <- count_core(grid_max, cand, dims)
    # tmp_min <- count_core(grid_min, cand, dims)
    n_unknown <- count_unknown(grid, cand, tmp_max$values)
    if (n_unknown == 0) next

    # minimum à cause du <- 0 plus loin. Bref, le minimum permet de forcer n_unknown = n_mines_around
    n_mines_around_max <- min(n_unknown, count_mines_left_around(grid_max, cand, tmp_max$values, dims))
    # n_mines_around_min <- count_mines_left_around(grid_min, cand, tmp_min$values, dims)
    empty_boxes_count <- empty_boxes_count + n_unknown - n_mines_around_max
    # full_boxes_count <- full_boxes_count + n_mines_around_min

    # si on sait déjà que trial contient trop de mines
    if (boxes_unknown - empty_boxes_count < trial) return("too many mines")
    # if (trial < full_boxes_count) return("not enough mines")
    counted[position_to_i_mat(tmp_max$positions, dims)] <- 1
  
    # faire comme si on connaissait ces boites, pour ne pas les recompter
    grid_max[counted == 1 & !grid_max %in% known] <- 0
    # grid_min[counted == 1 & !grid_min %in% known] <- flag_on_mine
  }
  NULL
}

