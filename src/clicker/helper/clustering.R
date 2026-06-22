source("src/game_engine/init_solved_around.R")
source("src/indicies/i_and_positions.R")
source("src/indicies/get_around_square.R")
source("src/indicies/square_pos_and_get_around_square.R")
source("src/clicker/probabilistic_clicker.R")

independant_clusters <- function(grid, solved_around, mines_left) {
  if (all(solved_around == -1)) {
    new_solved_around <- init_solved_around(grid, which(solved_around == -1))
    return(list(list(
      grid = grid,
      solved_around = new_solved_around,
      bornes_mines = c(mines_left, mines_left),
      possible = "NA"
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
            possible = rep("NA", diff(bornes_mines1) + 1)
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
    # TODO weird mais on va le permettre vu que parfois en hypothesis == 2 ca peut être impossible
    if (bornes_mines1[2] < bornes_mines1[1]) browser() # TODO solve
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
      possible = rep("NA", diff(bornes_mines1) + 1)
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

cluster_from_draft <- function(grid, solved_around, mines_left, clusters_cache, call_precise = TRUE, ...) {
  if (is.null(clusters_cache)) {
    clusters <- independant_clusters(grid, solved_around, mines_left)
    
    # recalculer les bornes précises des mines clusters
    if (call_precise) return(precise_clusters_bounds_all(grid, solved_around, mines_left, clusters, ...))
    return(clusters)
  }
  
  if (all(solved_around == -1)) {
    browser() # pas implémenté et pas sensé se rendre ici logiquement
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
        
        # utilisation de la cache
        i_clust_identical_in_cache <- which(sapply(
          clusters_cache$clusters,
          function(clust) all(clust$in_cluster == in_next_cluster) &&
            all(clust$grid[in_next_cluster] == grid[in_next_cluster])
        ))
        if (length(i_clust_identical_in_cache) > 0) {
          # réutiliser les infos du cluster
          bornes_mines1 <- clusters_cache$clusters[[i_clust_identical_in_cache]]$bornes_mines
          possible <- clusters_cache$clusters[[i_clust_identical_in_cache]]$possible
        } else {
          # compter les bornes de mines du cluster
          bornes_mines1 <- c(0, min(mines_left, sum(!grid[in_next_cluster] %in% known), na.rm = TRUE))
          possible <- rep("NA", diff(bornes_mines1) + 1)
        }
        
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
            possible = possible
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
    # TODO weird mais on va le permettre vu que parfois en hypothesis == 2 ca peut être impossible
    if (bornes_mines1[2] < bornes_mines1[1]) return(NULL) # TODO solve, je crois qu'en retournant NULL, j'invalide les
    # clusters cache et tous les 
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
  
  clusters <- list(
    # void est un groupe spécial de cases sans aucune information
    void = list(
      grid = tmp_grid,
      solved_around = new_solved_around,
      in_cluster = in_void,
      bornes_mines = bornes_mines1,
      possible = rep("NA", diff(bornes_mines1) + 1)
    ),
    known_but_does_nothing = list(in_cluster = known_but_does_nothing),
    clusters = groups
  )
  
  if (call_precise) return(precise_clusters_bounds_all(grid, solved_around, mines_left, clusters, ...))
  clusters
}
