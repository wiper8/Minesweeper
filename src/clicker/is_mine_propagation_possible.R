source("src/simulate_game.R")

is_mine_propagation_possible <- function(grid, total_mines = NA, solved_around) {
  over <- tryCatch(
    is_game_over(grid, total_mines),
    error = function(e) {
      "nombre invalide de mines"
    }
  )
  if (over == "nombre invalide de mines") return(FALSE)

  # simuler une nouvelle partie avec un clicker certain
  if (is_game_over(grid, total_mines) == 1) return(TRUE)
  propagated_game_end <- main_game_loop(grid, total_mines, certain_core, solved_around = solved_around)
  
  # le clicker ne savait plus quoi faire et la partie n'était pas terminée
  over <- tryCatch(
    is_game_over(grid, total_mines),
    error = function(e) {
      "nombre invalide de mines"
    }
  )
  if (over == "nombre invalide de mines") return(FALSE)
  if (over == 1) return(TRUE)
  if (!propagated_game_end[[2]] %in% c("win", "lost")) return(TRUE)
  if (propagated_game_end[[2]] == "win") return(TRUE)
  if (propagated_game_end[[2]] == "lost") return(FALSE)
  browser()
}
