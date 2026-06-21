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
    try_solve_a_cluster(clusters$clusters, 1, mines_left, grid, clusters$void$in_cluster, ...)
  }
}

try_solve_a_cluster <- function(clusters, clust_i, mines_left, grid, void, ...) {
  mines_target_ratio <- if (is.na(mines_left)) 0.5 else mines_left / sum(!grid %in% known)
  trials_order <- seq(clusters[[clust_i]]$bornes_mines[1], clusters[[clust_i]]$bornes_mines[2])
  # filtrer
  trials_order <- trials_order[clusters[[clust_i]]$possible %in% c("TRUE", "NA")]
  ratios <- trials_order / sum(!clusters[[clust_i]]$grid[clusters[[clust_i]]$solved_around != -1] %in% known)
  trials_order <- trials_order[order(abs(ratios - mines_target_ratio))]
  
  # essayer de résoudre le cluster avec `mines_trial`
  # simuler une nouvelle partie avec un clicker certain
  propagated_game_end <- main_game_loop(
    clusters[[clust_i]]$grid, trials_order[1], certain_core,
    solved_around = clusters[[clust_i]]$solved_around, hypothesis = 2, cluster = clusters[[clust_i]]$in_cluster,
    ...
  )
  
  # si ça fonctionne, tenter de résoudre les autres clusters en ajustant récursivement les mines restantes
  if (propagated_game_end[[2]] %in% c("win", "le clicker ne sait pu quoi faire")) {
    clusters[[clust_i]]$last_success_mines <- trials_order[1]
    clusters[[clust_i]]$possible[trials_order[1] - clusters[[clust_i]]$bornes_mines[1] + 1] <- "TRUE"
    
    if (clust_i == length(clusters)) { # on a atteint le dernier cluster à tester
      # dernière vérification que le total de mines utilisé est plausible
      nb_in_void_cluster <- sum(void)
      if (is.na(mines_left)) return(TRUE)
      if ((mines_left - trials_order[1]) >= 0 && (mines_left - trials_order[1]) <= nb_in_void_cluster) return(TRUE)
      return(FALSE) # pas un cas possible
    }
    if ((clust_i + 1) > length(clusters)) browser()
    clusters[[clust_i]]$possible[clusters[[clust_i]]$possible == "maybe next time"] <- "NA"
    return(try_solve_a_cluster(clusters, clust_i + 1, mines_left - trials_order[1], grid, void, ...))
  } else if (propagated_game_end[[2]] == "partie impossible") {
    clusters[[clust_i]]$possible[trials_order[1] - clusters[[clust_i]]$bornes_mines[1] + 1] <- "maybe next time"
    if (all(clusters[[clust_i]]$possible == "FALSE") || all(clusters[[clust_i]]$possible == "maybe next time")) {
      return(FALSE) # pas possible
    }
    return(try_solve_a_cluster(clusters, clust_i, mines_left, grid, void, ...))
  } else if (propagated_game_end[[2]] == "lost") {
    # rares situations (voir tests unitaires) où un mines_trial force un clicker certain de commettre une erreur
    # ex: en pensant qu'il ne reste plus de mines nul part donc qu'on peut cliquer n'importe où
    if (clust_i == length(clusters)) return(TRUE) # on a réussi
    if ((clust_i + 1) > length(clusters)) browser()
    return(try_solve_a_cluster(clusters, clust_i + 1, mines_left - trials_order[1], grid, void, ...))
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
  
  potential_cluster <- grid >= 0 & grid != flag_on_mine & solved_around == 0
  in_any_cluster <- matrix(FALSE, nrow = nrow(grid), ncol = ncol(grid))
  known_but_does_nothing <- in_any_cluster

  for (i in which(potential_cluster)) {
    if (!in_any_cluster[i]) {
      clust <- create_cluster_from_i(grid, i, solved_around)
      if (sum(clust) == 1) {
        known_but_does_nothing[i] <- TRUE
        in_any_cluster[i] <- TRUE
        next
      }
      # pas supposé que des boîtes soient dans plusieurs clusters, sauf les known
      if (any(in_any_cluster & clust & !grid %in% known)) browser()
      in_next_cluster <- clust == 1
      if (any(in_next_cluster)) {
        in_any_cluster <- in_any_cluster | in_next_cluster
        # compter les bornes de mines du cluster
        bornes_mines1 <- c(0, min(mines_left, sum(!grid[in_next_cluster] %in% known), na.rm = TRUE))

        tmp_grid <- grid
        tmp_grid[clust == 0] <- void_box
        # impossible de créer un cluster vide
        if (!all(tmp_grid == void_box)) {
          new_solved_around <- init_solved_around(tmp_grid, which(solved_around == -1))
          
          groups[[length(groups) + 1]] <- list(
            grid = tmp_grid,
            solved_around = new_solved_around,
            in_cluster = in_next_cluster,
            bornes_mines = bornes_mines1,
            possible = rep("NA", diff(bornes_mines1) + 1),
            last_success_mines = NA
          )
        }
      }
    }
  }
  
  if (length(groups) > length(grid)) browser() # pas sensé déclencher
  
  # parmi les cases restantes, les non clusterisées et connues sont known_but_does_nothing
  known_but_does_nothing <- known_but_does_nothing | (!in_any_cluster & grid %in% known)
  in_any_cluster <- in_any_cluster | known_but_does_nothing
  
  # le dernier cluster est le "void" inconnu
  # cacher les boxes non dans le cluster en cours
  in_void <- !in_any_cluster
  tmp_grid <- grid * 0 + void_box
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
    bornes_mines1[1] <- max(0, bornes_mines1[1], na.rm = TRUE)
    bornes_mines1[2] <- min(mines_left, bornes_mines1[2], sum(in_void), na.rm = TRUE)
  }
  
  # vérifier que chaque case est dans un et un seul cluster, sauf les known qui peuvent être réutilisés
  if (any(Reduce(
    `+`,
    append(
      lapply(groups, function(lst) lst$in_cluster),
      list(in_void)
    ) |>
    append(list(known_but_does_nothing))
  ) != 1 & !grid %in% known)) browser()
  
  list(
    # void est un groupe spécial de cases sans aucune information
    void = list(
      grid = tmp_grid,
      solved_around = new_solved_around,
      in_cluster = in_void,
      bornes_mines = bornes_mines1,
      possible = rep("NA", diff(bornes_mines1) + 1),
      last_success_mines = NA
    ),
    known_but_does_nothing = list(in_cluster = known_but_does_nothing),
    clusters = groups
  )
}

create_cluster_from_i <- function(grid, i, solved_around, in_cluster = NULL) {
  first_level <- is.null(in_cluster)
  if (is.null(in_cluster)) in_cluster <- grid * 0
  
  # est dans void : ne pas mettre de 1 dans in_cluster[i]
  if (solved_around[i] == -1 && !grid[i] %in% known) {
    return(in_cluster)
  }
  dims <- dim(grid)
  tmp <- square_pos_and_get_around_square(i_to_position(i, dims), grid, dims)
  positions <- tmp[[1]]
  
  # va dans la catégorie de in_cluster "known_but_does_nothing"
  tmp2 <- get_around_square(i_to_position(i, dims), solved_around, dims)
  if (first_level &&
      grid[i] %in% hp_brings_no_info_to_center_unknown &&
      sum(tmp[[2]] %in% known) == 1 &&
      all(tmp2 == -1)) {
    in_cluster[i] <- 1
    return(in_cluster)
  }
  
  in_cluster[i] <- 1
  
  if (grid[i] %in% known &&
      sum(!tmp[[2]] %in% known) == 0) return(in_cluster)
  
  if (grid[i] %in% hp_known_but_cannot_expand) return(in_cluster)
  
  if (!grid[i] %in% known) {
    potential_neighboords <- position_to_i_mat(positions, dims)
    # exclure les cases déjà dans le in_cluster
    potential_neighboords <- potential_neighboords[in_cluster[potential_neighboords] != 1]
    # conserver les cases connues et unsolved
    potential_neighboords <- potential_neighboords[solved_around[potential_neighboords] == 0 &
                                                     grid[potential_neighboords] %in% known &
                                                     !grid[potential_neighboords] %in% hp_brings_no_info_to_center_unknown]
    # ajouter les voisins qui apportent de l'info au reste du in_cluster
    for (j in potential_neighboords) {
      in_cluster <- create_cluster_from_i(
        grid,
        j,
        solved_around,
        in_cluster
      )
    }
    return(in_cluster)
  }
  
  potential_neighboords <- position_to_i_mat(positions, dims)
  # exclure les cases déjà dans le in_cluster
  potential_neighboords <- potential_neighboords[in_cluster[potential_neighboords] != 1]
  
  # ajouter les voisins qui apportent de l'info au reste du in_cluster
  for (j in potential_neighboords) {
    in_cluster <- create_cluster_from_i(
      grid,
      j,
      solved_around,
      in_cluster
    )
  }
  
  in_cluster
}

test_trial <- function(grid, mines_left, in_cluster, trial) {
  # préciser les bornes
  tmp_grid <- grid
  # pour simplifier, on met des no-mines partout ailleurs
  tmp_grid[clust == 0] <- void_box
  new_solved_around <- init_solved_around(tmp_grid)
  
  # résoudre le cluster avec `trial` mines
  res <- is_mine_propagation_possible(
    convert_grid_solution_to_human_grid(tmp_grid, new_solved_around),
    trial,
    new_solved_around,
    to_clusterise = FALSE,
    in_cluster = in_cluster
  )
  res <- as.logical(res)
  if (isFALSE(res)) return(FALSE)

  # résoudre le reste sans le cluster avec mines_left - trial mines
  tmp_grid <- grid
  tmp_grid[in_cluster == 1 & !grid %in% known] <- void_box # & !grid %in% known car sinon on pourrait masquer des cellules
  # essentielles aux autres clusters
  new_solved_around <- init_solved_around(tmp_grid)
  
  is_mine_propagation_possible(
    convert_grid_solution_to_human_grid(tmp_grid, new_solved_around),
    mines_left - trial,
    new_solved_around,
    to_clusterise = FALSE
  )
}
