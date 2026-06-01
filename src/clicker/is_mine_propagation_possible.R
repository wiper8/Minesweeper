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
    stop("erreur")
  }

  clusters <- independant_clusters(grid, solved_around, mines_left)
  if (length(clusters$clusters) <= 1) {
    propagated_game_end <- main_game_loop(grid, mines_left, certain_core, solved_around = solved_around,
                                          hypothesis = 2, ...)
    
    if (propagated_game_end[[2]] == "partie impossible") return(FALSE)
    if (propagated_game_end[[2]] == "le clicker ne sait pu quoi faire") return(TRUE)
    if (propagated_game_end[[2]] == "win") return(TRUE)
    if (propagated_game_end[[2]] == "lost") browser() # ne serait pas supposer perdre avec certain_core comme clicker
    browser()
    stop("erreur")
  } else {
    try_solve_a_cluster(clusters$clusters, 1, mines_left, grid, ...)
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
      in_void_cluster <- matrix(TRUE, nrow = nrow(clusters[[1]]$solved_around), ncol = ncol(clusters[[1]]$solved_around))
      for (mat in lapply(clusters, function(x) x$in_cluster)) {
        in_void_cluster <- in_void_cluster & !mat
      }
      nb_in_void_cluster <- sum(in_void_cluster)
      if (is.na(mines_left)) return(TRUE)
      if ((mines_left - trials_order[1]) >= 0 && (mines_left - trials_order[1]) <= nb_in_void_cluster) return(TRUE)
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

independant_clusters <- function(grid, solved_around, mines_left) {
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
  groups <- list()
  
  potential_cluster <- grid * 0 | (solved_around != -1) | grid %in% known
  in_any_cluster <- matrix(FALSE, nrow = nrow(grid), ncol = ncol(grid))
  known_but_does_nothing <- in_any_cluster

  for (i in which(potential_cluster)) {
    if (potential_cluster[i]) {
      clust <- create_cluster_from_i(grid, i)
      if (sum(clust) == 1) {
        known_but_does_nothing[i] <- TRUE
        in_any_cluster[i] <- TRUE
        next
      }
      if (any(in_any_cluster & clust)) browser() # pas supposé que des boîtes soient dans plusieurs clusters
      in_next_cluster <- !in_any_cluster & clust
      if (any(in_next_cluster)) {
        in_any_cluster <- in_any_cluster | in_next_cluster
        # compter les bornes de mines du cluster
        bornes_mines1 <- c(0, min(mines_left, sum(!grid[potential_cluster & in_next_cluster] %in% known), na.rm = TRUE))
        potential_cluster[potential_cluster] <- !clust[potential_cluster]
        
        tmp_grid <- grid
        tmp_grid[clust == 0] <- unknown_box
        # impossible de créer un cluster vide
        if (!all(tmp_grid == unknown_box)) {
          new_solved_around <- init_solved_around(tmp_grid, which(solved_around == -1))
          
          groups[[length(groups) + 1]] <- list(
            grid = tmp_grid,
            solved_around = new_solved_around,
            in_cluster = in_next_cluster,
            bornes_mines = bornes_mines1,
            possible = if (is.na(mines_left)) NA else rep("NA", diff(bornes_mines1) + 1),
            last_success_mines = NA
          )
        }
      }
    }
  }
  
  if (length(groups) > length(grid)) browser() # pas sensé déclencher

  # le dernier cluster est le "void" inconnu
  # cacher les boxes non dans le cluster en cours
  tmp_grid <- grid * 0 + unknown_box
  new_solved_around <- grid * 0 - 1
  if (length(groups) == 0) {
    bornes_mines1 <- c(mines_left, mines_left)
  } else {
    bornes_mines1 <- mines_left - c(
      sum(sapply(groups, function(clust) {
        clust$bornes_mines[2]
      })), 
      sum(sapply(groups, function(clust) {
        clust$bornes_mines[1]
      }))
    )
    bornes_mines1[1] <- max(0, bornes_mines1[1])
    bornes_mines1[2] <- min(mines_left, bornes_mines1[2])
  }
  in_void <- !in_any_cluster

  # vérifier que chaque case est dans un et un seul cluster
  if (any(Reduce(
    `+`,
    append(
      lapply(groups, function(lst) lst$in_cluster),
      list(in_void)
    ) |>
    append(list(known_but_does_nothing))
  )) != 1) browser()
  
  list(
    # void est un groupe spécial de cases sans aucune information
    void = list(
      grid = tmp_grid,
      solved_around = new_solved_around,
      in_cluster = in_void,
      bornes_mines = bornes_mines1,
      possible = if (is.na(mines_left)) NA else rep("NA", diff(bornes_mines1) + 1),
      last_success_mines = NA
    ),
    known_but_does_nothing = list(in_cluster = known_but_does_nothing),
    clusters = groups
  )
}

create_cluster_from_i <- function(grid, i, cluster = NULL) {
  if (is.null(cluster)) cluster <- grid * 0
  tmp <- square_pos_and_get_around_square(i_to_position(i, dim(grid)), grid)
  positions <- tmp[[1]]
  if (all(tmp[[2]] %in% hp_brings_no_info_to_center_unknown)) {
    if (grid[i] == unknown_box) {
      return(cluster)
    }
    cluster[i] <- 1
    return(cluster) # va dans la catégorie de cluster "known_but_does_nothing"
  }
  if (any(tmp[[2]] %in% known_but_no_flag)) {
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

test_trial <- function(grid, mines_left, clust, trial) {
  # préciser les bornes
  tmp_grid <- grid
  # pour simplifier, on met des no-mines partout ailleurs
  tmp_grid[clust == 0] <- unknown_box
  new_solved_around <- init_solved_around(tmp_grid)
  
  # résoudre le cluster avec `trial` mines
  res <- is_mine_propagation_possible(
    convert_grid_solution_to_human_grid(tmp_grid, new_solved_around),
    trial,
    new_solved_around,
    to_clusterise = FALSE,
    cluster = clust
  )
  res <- as.logical(res)
  
  # résoudre le reste sans le cluster avec mines_left - trial mines
  tmp_grid <- grid
  tmp_grid[clust == 1] <- flag_on_mine
  new_solved_around <- init_solved_around(tmp_grid)
  
  res &&
    is_mine_propagation_possible(
      convert_grid_solution_to_human_grid(tmp_grid, new_solved_around),
      mines_left - trial,
      new_solved_around,
      to_clusterise = FALSE
    )
}
