library(profvis)
source("src/simulate_game.R")
source("src/compute_probs_success.R")
source("src/plots.R")
source("src/clicker/random_clicker.R")
source("src/clicker/certain_else_random_clicker.R")

set.seed(2026L)
simulate_game(40, c(17, 9), random_clicker)
set.seed(2026L)
simulate_game(40, c(17, 9), certain_else_random_clicker)

set.seed(2026L)
compute_probs_success(n = 100, total_mines = 40, c(17, 9), certain_else_random_clicker)
compute_probs_success(n = 100, total_mines = 56, c(17, 9), certain_else_random_clicker)

# graphique
set.seed(2026L)
show_mines_difficulty(compare_clickers(n = 200, c(6, 4)))
show_mines_difficulty(compare_clickers(n = 200, c(17, 9)))

# TODO aussi évaluer le temps pour résoudre. Car je souhaite trouver l'espérance de temps avant une première solution 
# avec n mines, donc parfois plus de mines = essais plus courts = plus d'essais = pas nécessairement plus long
# si p = 2% avec t=3min, vs p=1% avec t=1min, 1/2% * 3 > 1/1% * 1

hypothesis_test(20, total_mines = 10, dims = c(17, 9), random_clicker, certain_else_random_clicker)
hypothesis_test(50, total_mines = 56, dims = c(17, 9), certain_else_random_clicker, human_clicker)

compute_probs_success(n = 100, total_mines = 7, c(10, 8), human_clicker) # begginner
compute_probs_success(n = 100, total_mines = 15, c(14, 9), human_clicker) # easy
compute_probs_success(n = 100, total_mines = 40, c(20, 15), human_clicker) # intermediate
compute_probs_success(n = 100, total_mines = 99, c(26, 19), human_clicker) # expert
