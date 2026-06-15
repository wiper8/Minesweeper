library(profvis)
source("src/simulate_game.R")
source("src/compute_probs_success.R")
source("src/plots.R")
source("src/clicker/random_clicker.R")
source("src/clicker/certain_else_random_clicker.R")
source("src/clicker/smart_clicker.R")

# simuler une partie
set.seed(2026L)
simulate_game(40, c(17, 9), random_clicker)
set.seed(2026L)
simulate_game(40, c(17, 9), certain_else_random_clicker)
set.seed(2026L)
simulate_game(40, c(17, 9), smart_clicker)

# comparer la vitesse de 2 sortes de clickers
set.seed(2026L)
compare_clickers_times(100, c(6, 4), total_mines = 6)
profvis(
  compute_probs_success(5, c(17, 9), total_mines = 35, smart_clicker2)
)

# calculer probabilités de succès
set.seed(2026L)
compute_probs_success(n = 200, total_mines = 40, c(17, 9), smart_clicker)
compute_probs_success(n = 200, total_mines = 56, c(17, 9), smart_clicker)

# graphique de la probabilité de succès selon le nombre de mines
set.seed(2026L)
show_mines_difficulty(compare_clickers(n = 10, c(6, 4)))
show_mines_difficulty(compare_clickers(n = 200, c(17, 9)))

# vérification statistique qu'un clicker est meilleur qu'un autre
hypothesis_test(10, total_mines = 25, dims = c(17, 9), smart_clicker, smart_clicker2)
hypothesis_test(20, total_mines = 20, dims = c(17, 9), random_clicker, certain_else_random_clicker)
hypothesis_test(50, total_mines = 56, dims = c(17, 9), certain_else_random_clicker, smart_clicker)

# pour optimiser où cliquer au début d'une partie (coin, côté ou centre)
set.seed(2026L)
show_first_click_probs(n = 1000, total_mines = 7, dims = c(7, 4)) # ~47.7 minutes
show_first_click_probs(n = 10, total_mines = 13, dims = c(10, 5))
show_first_click_probs(n = 100, total_mines = 40, dims = c(17, 9))

# estimer les probabilités de succès selon les niveaux préétablis d'une application
compute_probs_success(n = 100, total_mines = 7, c(10, 8), smart_clicker) # begginner
compute_probs_success(n = 100, total_mines = 15, c(14, 9), smart_clicker) # easy
compute_probs_success(n = 100, total_mines = 40, c(20, 15), smart_clicker) # intermediate
compute_probs_success(n = 100, total_mines = 99, c(26, 19), smart_clicker) # expert
