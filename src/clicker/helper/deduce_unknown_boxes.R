deduce_unknown_boxes <- function(grid, mines_left, clusters_cache, in_cluster = NULL, ...) {
  if (is.na(mines_left)) return(NULL)
  known_boxes <- grid %in% known
  
  if (!is.null(in_cluster)) {
    known_boxes <- known_boxes | !in_cluster
    no_info_boxes <- sum(!known_boxes)
    if (no_info_boxes < mines_left) return(list(clicks = "impossible", clusters_cache = clusters_cache))
    if (no_info_boxes == mines_left) return(list(clicks = NULL, clusters_cache = clusters_cache)) # ne sait simplement plus quoi cliquer dans les autres clusters
  }
  
  dims <- dim(grid)
  if (mines_left < 0) return(list(clicks = "impossible", clusters_cache = clusters_cache))
  if (mines_left == 0) return(list(
    clicks = lapply(which(!grid %in% known), function(i) list(i_to_position(i, dims), TRUE, "certain")),
    clusters_cache = clusters_cache
  ))
  
  no_info_boxes <- sum(!known_boxes)
  if (no_info_boxes < mines_left) return(list(clicks = "impossible", clusters_cache = clusters_cache))
  if (no_info_boxes == mines_left) return(list(
    clicks = list(list(i_to_position(which(!known_boxes)[1], dims), FALSE, "certain")),
    clusters_cache = clusters_cache
  ))
  
  solved_around <- init_solved_around(grid)
  # on a aucune information sur les boîtes
  if (all(solved_around != 0) && no_info_boxes > mines_left) {
    return(list(clicks = NULL, clusters_cache = clusters_cache))
  }
  res <- is_void_solvable(grid, solved_around, mines_left,
                          clusters = clusters_cache %||% independant_clusters(grid, solved_around, mines_left))
  if (!isFALSE(res$clicks)) {
    return(res)
  }

  list(clicks = NULL, clusters_cache = res$clusters_cache)
}

is_void_solvable <- function(grid, solved_around, mines_left, clusters) {
  res <- are_no_mine_in_void(grid, solved_around, mines_left, clusters)
  if (!isFALSE(res$clicks)) {
    return(res)
  }
  is_void_full_mines(grid, solved_around, mines_left, res$clusters_cache)
}

are_no_mine_in_void <- function(grid, solved_around, mines_left, clusters) {
  known_boxes <- grid %in% known
  
  # petit raccourci : s'il y a moins de mines que le nombre de boîtes à découvrir
  if (mines_left <= sum(solved_around == 0 & !known_boxes)) {
    dims <- dim(grid)
    if (length(clusters$clusters) == 0 && mines_left == 0) {
      next_i <- which(clusters$void$in_cluster)[1]
      return(list(clicks = list(list(i_to_position(next_i, dims), TRUE, "certain")), clusters_cache = clusters))
    }
    if (length(clusters$clusters) == 0) {
      return(list(clicks = FALSE, clusters_cache = clusters)) # que du void avec des mines
    }
    if (all(!clusters$void$in_cluster)) {
      return(list(clicks = FALSE, clusters_cache = clusters)) # pas de void
    }
    
    clusters_bornes_min_precises <- precise_clusters_bounds_min_shortcut(grid, solved_around, mines_left, clusters)
    clusters <- clusters_bornes_min_precises$clusters_cache
    # SHORTCUT
    # si toutes les mines sont assurément dans les clusters, je peux cliquer dans le vide
    if (isTRUE(sum(clusters_bornes_min_precises$nb_min) == mines_left)) {
      return(
        list(
          clicks = lapply(which(clusters$void$in_cluster), function(next_i) {
            list(i_to_position(next_i, dims), TRUE, "certain")
          }),
          clusters_cache = clusters
        )
        
      )
    }
  }
  list(clicks = FALSE, clusters_cache = clusters)
}

is_void_full_mines <- function(grid, solved_around, mines_left, clusters) {
  if (mines_left == 0) list(clicks = FALSE, clusters_cache = clusters)
  n_box_in_void <- sum(clusters$void$in_cluster)
  if (n_box_in_void > mines_left) return(list(clicks = FALSE, clusters_cache = clusters))
  if (n_box_in_void == 0) return(list(clicks = FALSE, clusters_cache = clusters))
  # if (all(!grid %in% c(-10, -11, -9, -8))) browser()
  clusters_bornes_max_precises <- precise_clusters_bounds_max_shortcut_full_void(grid, solved_around, mines_left, clusters)
  clusters <- clusters_bornes_max_precises$clusters_cache
  remaining_for_void <- mines_left - sum(clusters_bornes_max_precises$nb_max)
  
  dims <- dim(grid)
  if (isTRUE(n_box_in_void == remaining_for_void)) {
    return(
      list(
        clicks = lapply(which(clusters$void$in_cluster), function(next_i) {
          list(i_to_position(next_i, dims), FALSE, "certain")
        }),
        clusters_cache = clusters
      )
    )
  }
  list(clicks = FALSE, clusters_cache = clusters)
}

precise_clusters_bounds_min_shortcut <- function(grid, solved_around, mines_left, clusters) {
  # if (sum(grid == -10) == 0) browser()
  res <- numeric(length(clusters$clusters))
  cumul_min_shortcut <- 0
  for (i in seq_along(res)) {
    lst <- clusters$clusters[[i]]
    # cluster d'une seule case connue
    if (sum(lst$in_cluster) == 1) {
      res[i] <- 0
    } else {
      if (any(lst$possible == "TRUE")) {
        # raccourci : commencer au true et aller vers la gauche
        trials <- lst$bornes_mines[1]:lst$bornes_mines[2]
        right <- head(which(lst$possible == "TRUE"), 1)
        left <- 1
        min_possible <- NA
        while (left <= right) {
          if (lst$possible[right] == "TRUE") {
            possibility <- TRUE
          } else if (lst$possible[right] == "FALSE") {
            possibility <- FALSE
          } else {
            possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[right])
            clusters$clusters[[i]]$possible[right] <- possibility
          }
          
          if (possibility) {
            min_possible <- trials[right]
            right <- right - 1
          } else {
            break
          }
        }
      } else {
        trials <- lst$bornes_mines[1]:lst$bornes_mines[2]
        if (sum(grid == -10) == 0 && any(lst$possible == "TRUE" | lst$possible == "FALSE")) browser()
        left <- 1
        right <- length(trials)
        min_possible <- NA
        while (left <= right) {
          if (lst$possible[left] == "TRUE") {
            possibility <- TRUE
          } else if (lst$possible[left] == "FALSE") {
            possibility <- FALSE
          } else {
            possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[left])
            clusters$clusters[[i]]$possible[left] <- possibility
          }
          
          if (possibility) {
            min_possible <- trials[left]
            break
          } else {
            left <- left + 1
          }
        }
      }
      
      res[i] <- min_possible
    }
    cumul_min_shortcut <- cumul_min_shortcut + res[i]
    if (cumul_min_shortcut > mines_left) {
      # volontairement retourner un nombre erroné pour pas que le SHORTCUT dans are_no_mine_in_void déclenche
      return(list(nb_min = NA, clusters_cache = clusters))
    }
  }
  list(nb_min = res, clusters_cache = clusters)
}

precise_clusters_bounds_max_shortcut_full_void <- function(grid, solved_around, mines_left, clusters) {
  n_box_in_void <- sum(clusters$void$in_cluster)
  res <- numeric(length(clusters$clusters))
  cumul_max_shortcut <- 0
  for (i in seq_along(res)) {
    lst <- clusters$clusters[[i]]
    # cluster d'une seule case connue
    if (sum(lst$in_cluster) == 1) {
      res[i] <- 0
    } else {
      if (any(lst$possible == "TRUE")) {
        # raccourci : commencer au true et aller vers la droite
        trials <- lst$bornes_mines[1]:lst$bornes_mines[2]
        left <- tail(which(lst$possible == "TRUE"), 1)
        right <- length(trials)
        max_possible <- NA
        while (left <= right) {
          if (lst$possible[left] == "TRUE") {
            possibility <- TRUE
          } else if (lst$possible[left] == "FALSE") {
            possibility <- FALSE
          } else {
            possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[left])
            clusters$clusters[[i]]$possible[left] <- possibility
          }
          
          if (possibility) {
            max_possible <- trials[left]
            left <- left + 1
          } else {
            break
          }
        }
      } else {
        trials <- lst$bornes_mines[1]:lst$bornes_mines[2]
        right <- length(trials)
        max_possible <- NA
        while (1 <= right) {
          if (lst$possible[right] == "TRUE") {
            possibility <- TRUE
          } else if (lst$possible[right] == "FALSE") {
            possibility <- FALSE
          } else {
            possibility <- test_trial(grid, mines_left, lst$in_cluster, trials[right])
            clusters$clusters[[i]]$possible[right] <- possibility
          }
          
          if (possibility) {
            max_possible <- trials[right]
            break
          } else {
            right <- right - 1
          }
        }
      }
      
      res[i] <- max_possible
    }
    
    cumul_max_shortcut <- cumul_max_shortcut + res[i]
    if ((mines_left - cumul_max_shortcut) < n_box_in_void) {
      # volontairement retourner un nombre erroné pour pas que le SHORTCUT dans is_void_full_mines déclenche
      return(list(nb_max = NA, clusters_cache = clusters))
    }
  }
  list(nb_max = res, clusters_cache = clusters)
}
