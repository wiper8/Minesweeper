source("src/simulate_game.R")

#' À partir d'une hypothèse de mines, continuer la partie et évaluer s'il y aura une incohérence ou non
#'
is_mine_propagation_possible <- function(grid, mines_left = NA, solved_around) {
  if (!is_grid_possible(grid)) return(FALSE)
  if (is_game_over(grid, mines_left) == 1) return(TRUE)
  
  # simuler une nouvelle partie avec un clicker certain
  propagated_game_end <- main_game_loop(grid, mines_left, certain_core, solved_around = solved_around, hypothesis = TRUE)
  
  if (propagated_game_end[[2]] == "partie impossible") return(FALSE)
  if (propagated_game_end[[2]] == "le clicker ne sait pu quoi faire") return(TRUE)
  if (propagated_game_end[[2]] == "win") return(TRUE)
  if (propagated_game_end[[2]] == "lost") browser() # ne serait pas supposer perdre avec certain_core comme clicker
  browser()
}
