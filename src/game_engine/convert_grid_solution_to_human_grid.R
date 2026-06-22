convert_grid_solution_to_human_grid <- function(grid, solved_around = init_solved_around(grid), ...) {
  human_grid <- grid
  human_grid[solved_around == -1 & !grid %in% known] <- unknown_box
  human_grid[human_grid %in% hp_to_hypo_no_mine] <- unknown_box
  human_grid
}
