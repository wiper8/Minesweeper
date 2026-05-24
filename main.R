library(profvis)
source("src/simulate_game.R")
source("src/compute_probs_success.R")
source("src/plots.R")
source("src/clicker/random_clicker.R")
source("src/clicker/certain_else_random_clicker.R")

# simuler une partie
set.seed(2026L)
simulate_game(40, c(17, 9), random_clicker)
set.seed(2026L)
simulate_game(40, c(17, 9), certain_else_random_clicker)

# calculer probabilités de succès
set.seed(2026L)
compute_probs_success(n = 200, total_mines = 40, c(17, 9), certain_else_random_clicker)
compute_probs_success(n = 2500, total_mines = 56, c(17, 9), certain_else_random_clicker) # TODO exécuter 15 mins

# graphique de la probabilité de succès selon le nombre de mines
set.seed(2026L)
show_mines_difficulty(compare_clickers(n = 100, c(6, 4)))
show_mines_difficulty(compare_clickers(n = 120, c(17, 9))) # ~2h

# vérification statistique qu'un clicker est meilleur qu'un autre
hypothesis_test(20, total_mines = 20, dims = c(17, 9), random_clicker, certain_else_random_clicker)
hypothesis_test(50, total_mines = 56, dims = c(17, 9), certain_else_random_clicker, human_clicker)

# pour optimiser où cliquer au début d'une partie (coin, côté ou centre)
set.seed(2026L)
show_first_click_probs(n = 1000, total_mines = 7, dims = c(7, 4))
show_first_click_probs(n = 200, total_mines = 13, dims = c(10, 5))
show_first_click_probs(n = 200, total_mines = 40, dims = c(17, 9)) # ~2h

# estimer les probabilités de succès selon les niveaux préétablis d'une application
compute_probs_success(n = 100, total_mines = 7, c(10, 8), human_clicker) # begginner
compute_probs_success(n = 100, total_mines = 15, c(14, 9), human_clicker) # easy
compute_probs_success(n = 100, total_mines = 40, c(20, 15), human_clicker) # intermediate
compute_probs_success(n = 100, total_mines = 99, c(26, 19), human_clicker) # expert
