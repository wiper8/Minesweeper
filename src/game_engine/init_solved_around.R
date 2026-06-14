source("src/game_engine/apply_action.R")

init_solved_around <- function(grid, skip_i = NULL, old_solved = NULL) {
  solved_around <- if (is.null(old_solved)) grid * 0 - 1 else old_solved
  to_itr <- if (is.null(skip_i)) seq_len(length(grid)) else fast_setdiff_no_unique(seq_len(length(grid)), skip_i)
  for (i in to_itr) {
    solved_around <- update_solved_around(grid, solved_around, i, around_too = FALSE)
  }
  solved_around
}
