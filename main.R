library(profvis)
source("src/simulate_game.R")
source("src/plots.R")
source("src/clicker/random_clicker.R")
source("src/clicker/certain_else_random_clicker.R")
source("src/clicker/smart_clicker.R")
source("src/get/get_prob.R")
source("src/get/get_mines_difficulty.R")
source("src/get/get_first_click_probs.R")

# simuler une partie
set.seed(2026L)
simulate_game(40, c(17, 9), random_clicker)
set.seed(2026L)
simulate_game(40, c(17, 9), certain_else_random_clicker)
set.seed(2026L)
simulate_game(40, c(17, 9), smart_clicker)

# calculer probabilités de succès
compute_probs_success(n = 10, total_mines = 12, c(17, 9), smart_clicker) # exemple
get_prob(n = 200, total_mines = 40, c(17, 9))
get_prob(n = 200, total_mines = 56, c(17, 9))

# graphique de la probabilité de succès selon le nombre de mines
compare_clickers(n = 5, c(6, 4)) |> show_mines_difficulty() # exemple
get_mines_difficulty(n = 300, c(5, 2)) |> show_mines_difficulty()
get_mines_difficulty(n = 200, c(8, 4)) |> show_mines_difficulty()
get_mines_difficulty(n = 200, c(17, 9)) |> show_mines_difficulty()

# vérification statistique qu'un clicker est meilleur qu'un autre
hypothesis_test(20, total_mines = 5, dims = c(5, 9), random_clicker, certain_else_random_clicker) # exemple
hypothesis_test(20, total_mines = 20, dims = c(17, 9), random_clicker, certain_else_random_clicker) # CONFIRMÉ
hypothesis_test(100, total_mines = 30, dims = c(17, 9), certain_else_random_clicker, smart_clicker) # CONFIRMÉ

# pour optimiser où cliquer au début d'une partie (coin, côté ou centre)
first_click_probs(n = 4, total_mines = 5, dims = c(3, 4)) |> show_first_click_probs() # exemple
get_first_click_probs(n = 500, total_mines = 7, dims = c(3, 3)) |> show_first_click_probs() # exemple
get_first_click_probs(n = 500, total_mines = 7, dims = c(3, 3)) |> show_first_click_probs(legend_type = FALSE) # exemple
get_first_click_probs(n = 1000, total_mines = 4, dims = c(4, 4)) |> show_first_click_probs()
get_first_click_probs(n = 200, total_mines = 7, dims = c(7, 4)) |> show_first_click_probs()
get_first_click_probs(n = 100, total_mines = 13, dims = c(10, 5)) |> show_first_click_probs()
get_first_click_probs(n = 100, total_mines = 40, dims = c(17, 9)) |> show_first_click_probs()

# estimer les probabilités de succès selon les niveaux préétablis d'une application
get_prob(n = 1000, total_mines = 7, c(10, 8)) # begginner
get_prob(n = 500, total_mines = 15, c(14, 9)) # easy
get_prob(n = 400, total_mines = 40, c(20, 15)) # intermediate
get_prob(n = 160, total_mines = 99, c(26, 19)) # expert

show_box_probs(
  matrix(c(
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10,  1,  1, -10, -10, -10, -10, -10,
    -10, -10,  1, -10, -10, -10, -10, -10, -10,
    -10, -10,  2, -10, -10, -10, -10, -10, -10,
    -10, -10, -10,  2,  2,  2, -10, -10, -10,
    -10, -10,  3, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10, -10
  ), nrow = 17, byrow = TRUE),
  mines_left = 40
)
