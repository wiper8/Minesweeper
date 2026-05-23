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
compute_probs_success(n = 1000, total_mines = 56, c(17, 9), random_clicker)

# TODO retirer cette section ci-dessous, c'est pour générer un bug précis où le nb de combins est trop élevé
n <- 28
time <- rep(NA, n)
completed <- rep(NA, n)
set.seed(2026L)
for (i in seq_len(n)) {
  print(i)
  a <- Sys.time()
  tmp <- simulate_game(40, c(17, 9), certain_else_random_clicker)
  b <- Sys.time()
  time[i] <- as.numeric(difftime(b, a, units = "secs"))
  completed[i] <- mean(tmp[[1]] %in% known)
}

summary(time)
ggplot() +
  geom_histogram(aes(x = time))

ggplot() +
  geom_point(aes(x = time, y = completed))

set.seed(2026L)
compute_probs_success(n = 100, total_mines = 40, c(17, 9), certain_else_random_clicker)
compute_probs_success(n = 100, total_mines = 56, c(17, 9), certain_else_random_clicker)

# graphique
show_mines_difficulty(compare_clickers(n = 200, c(6, 4)))
show_mines_difficulty(compare_clickers(n = 200, c(17, 9)))

hypothesis_test(20, total_mines = 56, dims = c(17, 9), random_clicker, certain_else_random_clicker)
hypothesis_test(50, total_mines = 56, dims = c(17, 9), certain_else_random_clicker, human_clicker)

compute_probs_success(n = 100, total_mines = 7, c(10, 8), human_clicker) # begginner
compute_probs_success(n = 100, total_mines = 15, c(14, 9), human_clicker) # easy
compute_probs_success(n = 100, total_mines = 40, c(20, 15), human_clicker) # intermediate
compute_probs_success(n = 100, total_mines = 99, c(26, 19), human_clicker) # expert
