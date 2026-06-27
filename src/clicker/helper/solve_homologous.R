source("src/simulate_game.R")
source("src/game_engine/apply_action.R")
source("src/indicies/i_and_positions.R")
source("src/clicker/helper/homologous_next_i.R")


solve_homologous <- function(grid, mines_left, solved_around, in_cluster, homologous, ...) {
  dims <- dim(grid)
  grid_init <- grid
  mines_init <- mines_left
  solved_around_init <- solved_around

  # 1. trouver les nombres possibles de mines pour les homologues
  possibles_mines_homologous <- 0:min(mines_left, length(homologous$homologous_i), homologous$max_mines)

  # 2. énumérer toutes les combinaisons de mines
  combins <- lapply(possibles_mines_homologous, function(nb_mines_in_homologous) {
    grid <- grid_init
    mines_left <- mines_init
    solved_around <- solved_around_init

    # 3. compléter la grille pour TOUS les homologues avec une seule combinaison par nb de mines
    for (i in seq_len(nb_mines_in_homologous)) {
      # apposer une mine temporaire
      tmp <- apply_action(grid, i_to_position(homologous$homologous_i[i], dims), action = FALSE, mines_left,
                          solved_around, ...)
      grid <- tmp[[1]]
      mines_left <- tmp[[3]]
      solved_around <- tmp[[4]]
      if (tmp[[2]] == -1) browser() # pas sensé avoir perdu à ce point-ci
    }
    for (i in tail(seq_len(length(homologous$homologous_i)), length(homologous$homologous_i) - nb_mines_in_homologous)) {
      # apposer une no_mine temporaire
      tmp <- apply_action(grid, i_to_position(homologous$homologous_i[i], dims), action = TRUE, mines_left,
                          solved_around, ...)
      grid <- tmp[[1]]
      mines_left <- tmp[[3]]
      solved_around <- tmp[[4]]
      if (tmp[[2]] == -1) browser() # pas sensé avoir perdu à ce point-ci
    }

    # propager la partie
    tmp <- main_game_loop(grid, mines_left, certain_core, solved_around, hypothesis = 2)
    # 4. calculer les probs
    if (tmp[[2]] == "partie impossible") {
      return(list(n_combins = 0, probs = 0))
    }

    if (tmp[[2]] == "le clicker ne sait pu quoi faire") {
      clusters <- independant_clusters(tmp[[1]], tmp[[3]], tmp[[4]])
      if (length(clusters$clusters) == 1) {
        args <- list(...)
        args$in_cluster <- clusters$clusters[[1]]$in_cluster

        return(
          do.call(
            generate_probs_knowing_mines,
            append(
              args,
              list(
                grid_tmp_propagate = tmp[[1]],
                mines_left_init = tmp[[4]],
                solved_around = tmp[[3]]
              )
            )
          )
        )
      }
      return(
        compute_grid_probabilities(
          grid = tmp[[1]],
          mines_left = tmp[[4]],
          solved_around = tmp[[3]],
          return_n_combins = TRUE,
          ...
        )
      )
    }

    if (tmp[[2]] == "win") {
      probs <- tmp[[1]] %in% hp_flags
      # overwrite homologous
      probs[homologous$homologous_i] <- sum(probs[homologous$homologous_i]) / length(homologous$homologous_i)

      return(list(
        n_combins = 1,
        probs = probs
      ))
    }
    browser() # ne devrait pas être possible de se rendre ici
  })


  # 5. recombiner les probs et les combinaisons
  real_combins <- mapply(function(lst, n) lst$n_combins * choose(length(homologous$homologous_i), n), combins, possibles_mines_homologous)

  total_combins <- sum(real_combins)

  probs <- Reduce(
    `+`,
    mapply(function(lst, n) n * lst$probs, combins, real_combins, SIMPLIFY = FALSE)
  ) / total_combins

  probs[homologous$homologous_i] <- sum(probs[homologous$homologous_i]) / length(probs[homologous$homologous_i])

  return(list(
    n_combins = total_combins,
    probs = probs
  ))
}
