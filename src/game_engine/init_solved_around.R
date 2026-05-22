source("src/game_engine/apply_action.R")

init_solved_around <- function(grid) {
  solved_around <- grid * 0 - 1
  for (i in seq_len(length(grid))) {
    solved_around <- update_solved_around(grid, solved_around, i, around_too = FALSE)
  }
  solved_around
}
