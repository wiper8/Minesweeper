source("src/simulate_game.R")

#' À partir d'une hypothèse de mines, continuer la partie et évaluer s'il y aura une incohérence ou non
#'
is_mine_propagation_possible <- function(grid, total_mines = NA, solved_around) {
  if (!is_grid_possible(grid)) return(FALSE)

  # le clicker ne savait plus quoi faire et la partie n'était pas terminée
  over <- tryCatch(
    is_game_over(grid, total_mines),
    error = function(e) {
      "nombre invalide de mines"
    }
  )
  
  if (over == "nombre invalide de mines") return(FALSE)
  if (over == 1) return(TRUE)
  
  # simuler une nouvelle partie avec un clicker certain
  propagated_game_end <- main_game_loop(grid, total_mines, certain_core, solved_around = solved_around, hypothesis = TRUE)
  
  if (propagated_game_end[[2]] == "partie impossible") return(FALSE)
  if (propagated_game_end[[2]] == "le clicker ne sait pu quoi faire") return(TRUE)
  if (propagated_game_end[[2]] == "win") return(TRUE)
  if (propagated_game_end[[2]] == "lost") browser() # sinon FALSE
  browser()
}
