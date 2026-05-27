source("src/fast_setdiff.R")
source("src/simulate_game.R")
source("src/game_engine/init_solved_around.R")

#' À partir d'une hypothèse de mines, continuer la partie et évaluer s'il y aura une incohérence ou non
#'
is_mine_propagation_possible <- function(grid, mines_left = NA, solved_around, to_clusterise = TRUE, ...) {
  if (!is_grid_possible(grid)) return(FALSE)
  if (is_game_over(grid, mines_left) == 1) return(TRUE)

  if (!to_clusterise) {
    propagated_game_end <- main_game_loop(grid, mines_left, certain_core, solved_around = solved_around,
                                          hypothesis = 2, ...)
    
    if (propagated_game_end[[2]] == "partie impossible") return(FALSE)
    if (propagated_game_end[[2]] == "le clicker ne sait pu quoi faire") return(TRUE)
    if (propagated_game_end[[2]] == "win") return(TRUE)
    if (propagated_game_end[[2]] == "lost") browser() # ne serait pas supposer perdre avec certain_core comme clicker
    browser()
  }

  clusters <- independant_clusters(grid, solved_around, mines_left)
  if (length(clusters) == 1) {
    propagated_game_end <- main_game_loop(grid, mines_left, certain_core, solved_around = solved_around,
                                          hypothesis = 2, ...)
    
    if (propagated_game_end[[2]] == "partie impossible") return(FALSE)
    if (propagated_game_end[[2]] == "le clicker ne sait pu quoi faire") return(TRUE)
    if (propagated_game_end[[2]] == "win") return(TRUE)
    if (propagated_game_end[[2]] == "lost") browser() # ne serait pas supposer perdre avec certain_core comme clicker
    browser()
  } else {
    try_solve_a_cluster(clusters, 1, mines_left, grid, ...)
  }
}

try_solve_a_cluster <- function(clusters, clust_i, mines_left, grid, ...) {
  mines_target_ratio <- if (is.na(mines_left)) 0.5 else mines_left / sum(!grid %in% known)
  trials_order <- seq(clusters[[clust_i]]$bornes_mines[1], clusters[[clust_i]]$bornes_mines[2])
  # filtrer
  trials_order <- trials_order[clusters[[clust_i]]$possible %in% c("oui", "NA")]
  ratios <- trials_order / sum(!clusters[[clust_i]]$grid[clusters[[clust_i]]$solved_around != -1] %in% known)
  trials_order <- trials_order[order(abs(ratios - mines_target_ratio))]
  
  
  # essayer de résoudre le cluster avec `mines_trial`
  # simuler une nouvelle partie avec un clicker certain
  propagated_game_end <- main_game_loop(
    clusters[[clust_i]]$grid, trials_order[1], certain_core,
    solved_around = clusters[[clust_i]]$solved_around, hypothesis = 2, ...
  )
  
  # si ça fonctionne, tenter de résoudre les autres clusters en ajustant récursivement les mines restantes
  if (propagated_game_end[[2]] %in% c("win", "le clicker ne sait pu quoi faire")) {
    clusters[[clust_i]]$last_success_mines <- trials_order[1]
    clusters[[clust_i]]$possible[trials_order[1] - clusters[[clust_i]]$bornes_mines[1] + 1] <- "oui"
    
    if (clust_i == length(clusters)) { # on a atteint le dernier cluster à tester
      # dernière vérification que le total de mines utilisé est plausible
      no_cluster <- matrix(TRUE, nrow = nrow(clusters[[1]]$solved_around), ncol = ncol(clusters[[1]]$solved_around))
      for (mat in lapply(clusters, function(x) x$solved_around == -1)) {
        no_cluster <- no_cluster & mat
      }
      in_no_cluster <- sum(no_cluster)
      
      if ((mines_left - trials_order[1]) >= 0 && (mines_left - trials_order[1]) <= in_no_cluster) return(TRUE)
      return(FALSE) # pas un cas possible
    }
    if ((clust_i + 1) > length(clusters)) browser()
    clusters[[clust_i]]$possible[clusters[[clust_i]]$possible == "maybe next time"] <- "NA"
    return(try_solve_a_cluster(clusters, clust_i + 1, mines_left - trials_order[1], grid, ...))
  } else if (propagated_game_end[[2]] == "partie impossible") {
    clusters[[clust_i]]$possible[trials_order[1] - clusters[[clust_i]]$bornes_mines[1] + 1] <- "maybe next time"
    if (all(clusters[[clust_i]]$possible == "non")) return(FALSE) # pas possible
    return(try_solve_a_cluster(clusters, clust_i, mines_left, grid, ...))
  } else if (propagated_game_end[[2]] == "lost") {
    # rares situations (voir tests unitaires) où un mines_trial force un clicker certain de commettre une erreur
    # ex: en pensant qu'il ne reste plus de mines nul part donc qu'on peut cliquer n'importe où
    if (clust_i == length(clusters)) return(TRUE) # on a réussi
    if ((clust_i + 1) > length(clusters)) browser()
    return(try_solve_a_cluster(clusters, clust_i + 1, mines_left - trials_order[1], grid, ...))
  } else {
    browser() # si propgated_game_end a une fin inattendue
  }
}

independant_clusters <- function(grid, solved_around, mines_left, precise_bounds = FALSE) {
  if (all(solved_around == -1)) {
    new_solved_around <- init_solved_around(grid, which(solved_around == -1))
    return(list(list(
      grid = grid,
      solved_around = new_solved_around,
      bornes_mines = c(mines_left, mines_left),
      possible = "NA",
      last_success_mines = NA
    )))
  }
  
  res <- list()
  potential_cluster <- grid * 0 + (solved_around != -1)
  for (i in which(potential_cluster == 1)) {
    if (potential_cluster[i] == 1) {
      clust <- create_cluster_from_i(grid, i)
      if (any(clust == 1)) {

        # compter les bornes de mines
        bornes_mines1 <- c(0, sum(!grid[potential_cluster == 1 & clust == 1] %in% known))

        potential_cluster[potential_cluster == 1] <- 1 - clust[potential_cluster == 1]

        # cacher les boxes non dans le cluster en cours
        tmp_grid <- grid
        tmp_grid[clust == 0] <- -10

        if (all(tmp_grid == -10)) browser() # impossible de créer un cluster vide

        new_solved_around <- init_solved_around(tmp_grid, which(solved_around == -1))

        if (precise_bounds) {
          # préciser les bornes
          mines_target_ratio <- if (is.na(mines_left)) 0.5 else mines_left / sum(!tmp_grid %in% known)
          tmp_mines_left_min <- bornes_mines1[1]
          possibilities <- rep(NA, diff(bornes_mines1) + 1)
          trials <- bornes_mines1[1]:bornes_mines1[2]
          for (n in seq_along(trials)) {
            possibilities[n] <- is_mine_propagation_possible(
              tmp_grid,
              trials[n],
              new_solved_around,
              to_clusterise = FALSE
            )
          }
          if (all(!possibilities)) browser() # impossible
          keep <- c(which(possibilities)[1], tail(which(possibilities), 1))
          bornes_mines1 <- trials[keep]
          possibilities <- possibilities[keep[1]:keep[2]]
        } else {
          possibilities <- rep("NA", diff(bornes_mines1) + 1)
        }

        res[[length(res) + 1]] <- list(
          grid = tmp_grid,
          solved_around = new_solved_around,
          bornes_mines = bornes_mines1,
          possible = possibilities,
          last_success_mines = NA
        )
      }
    }
  }
  
  res
}

create_cluster_from_i <- function(grid, i, cluster = NULL) {
  if (is.null(cluster)) cluster <- grid * 0
  tmp <- square_pos_and_get_around_square(i_to_position(i, dim(grid)), grid)
  positions <- tmp[[1]]
  if (any(tmp[[2]] > 0)) {
    cluster[i] <- 1
    potential_neighboords <- position_to_i_mat(positions, dim(grid))

    # exclure les cases déjà dans le cluster
    potential_neighboords <- potential_neighboords[cluster[potential_neighboords] != 1]

    # exclure les voisins inconnus ou qui n'apporte pas d'information possible
    if (grid[i] %in% hp_brings_no_info_to_center_unknown) {
      potential_neighboords <- potential_neighboords[!grid[potential_neighboords] %in% hp_brings_no_info_to_center_unknown]
    }
    # voie rapide pour les voisins dévoilés : automatiquements ajoutés au cluster
    cluster[potential_neighboords][grid[potential_neighboords] > 0] <- 1
    for (j in potential_neighboords) {
      cluster <- create_cluster_from_i(
        grid,
        j,
        cluster
      )
    }
  }
  cluster
}
