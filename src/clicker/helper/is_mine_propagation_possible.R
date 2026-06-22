source("src/fast_setdiff.R")
source("src/simulate_game.R")
source("src/game_engine/init_solved_around.R")
source("src/clicker/helper/clustering.R")

#' À partir d'une hypothèse de mines, continuer la partie et évaluer s'il y aura une incohérence ou non
#'
is_mine_propagation_possible <- function(grid, mines_left = NA, solved_around, to_clusterise = TRUE, clusters_cache = NULL, ...) {
  if (!is_grid_possible(grid)) return(list(possible = FALSE, clusters_cache = clusters_cache))
  if (is_game_over(grid, mines_left) == 1) return(list(possible = TRUE, clusters_cache = clusters_cache))

  if (to_clusterise) {
    new_clusters <- cluster_from_draft(grid, solved_around, mines_left, clusters_cache, call_precise = FALSE, ...)
    # protection en cas de cas impossibles (hypothesis == 2 implicite pour que le NULL soit possible)
    if (is.null(new_clusters)) return(list(possible = FALSE, clusters_cache = clusters_cache))
  }

  if (!to_clusterise || length(new_clusters$clusters) <= 1) {
    if (to_clusterise) clusters_cache <- new_clusters
    propagated_game_end <- main_game_loop(grid, mines_left, certain_core, solved_around = solved_around,
                                          hypothesis = 2, clusters_cache = clusters_cache, ...)
    if (propagated_game_end[[2]] == "partie impossible") return(list(possible = FALSE, clusters_cache = clusters_cache))
    if (propagated_game_end[[2]] == "le clicker ne sait pu quoi faire") return(list(possible = TRUE, clusters_cache = clusters_cache))
    if (propagated_game_end[[2]] == "win") return(list(possible = TRUE, clusters_cache = clusters_cache))
    if (propagated_game_end[[2]] == "lost") browser() # ne serait pas supposer perdre avec certain_core comme clicker
    browser()
    stop("erreur")
  }

  tmp <- try_solve_a_cluster(new_clusters$clusters, 1, mines_left, grid, new_clusters$void$in_cluster,
                             clusters_cache = new_clusters, ...)
  new_clusters$clusters <- tmp$clusters
  list(
    possible = tmp$possible,
    clusters_cache = new_clusters
  )
}

try_solve_a_cluster <- function(clusters, clust_i, mines_left, grid, void, ...) {
  mines_target_ratio <- if (is.na(mines_left)) 0.5 else mines_left / sum(!grid %in% known)
  trials_order <- seq(clusters[[clust_i]]$bornes_mines[1], clusters[[clust_i]]$bornes_mines[2])
  # filtrer
  trials_order <- trials_order[clusters[[clust_i]]$possible == "NA"]
  if (length(trials_order) == 0) {
    # vérifier qu'il y a au moins un TRUE
    if (any(clusters[[clust_i]]$possible == "TRUE")) return(list(possible = TRUE, clusters = clusters))
    browser()
  }
  ratios <- trials_order / sum(!clusters[[clust_i]]$grid[clusters[[clust_i]]$solved_around != -1] %in% known)
  trials_order <- trials_order[order(abs(ratios - mines_target_ratio))]

  # essayer de résoudre le cluster avec `mines_trial`
  # simuler une nouvelle partie avec un clicker certain
  propagated_game_end <- main_game_loop(
    clusters[[clust_i]]$grid, trials_order[1], certain_core,
    solved_around = clusters[[clust_i]]$solved_around, hypothesis = 2, in_cluster = clusters[[clust_i]]$in_cluster,
    ...
  )

  # si ça fonctionne, tenter de résoudre les autres clusters en ajustant récursivement les mines restantes
  if (propagated_game_end[[2]] %in% c("win", "le clicker ne sait pu quoi faire")) {
    clusters[[clust_i]]$possible[trials_order[1] - clusters[[clust_i]]$bornes_mines[1] + 1] <- "TRUE"

    if (clust_i == length(clusters)) { # on a atteint le dernier cluster à tester
      # dernière vérification que le total de mines utilisé est plausible
      nb_in_void_cluster <- sum(void)
      if (is.na(mines_left)) return(list(possible = TRUE, clusters = clusters))
      if ((mines_left - trials_order[1]) >= 0 && (mines_left - trials_order[1]) <= nb_in_void_cluster) return(list(possible = TRUE, clusters = clusters))
      clusters[[clust_i]]$possible[clusters[[clust_i]]$possible == "maybe next time"] <- "NA"
      return(list(possible = FALSE, clusters = clusters)) # pas un cas possible
    }
    if ((clust_i + 1) > length(clusters)) browser()
    return(try_solve_a_cluster(clusters, clust_i + 1, mines_left - trials_order[1], grid, void, ...))
  } else if (propagated_game_end[[2]] == "partie impossible") {
    clusters[[clust_i]]$possible[trials_order[1] - clusters[[clust_i]]$bornes_mines[1] + 1] <- "maybe next time"
    if (all(clusters[[clust_i]]$possible == "FALSE") || all(clusters[[clust_i]]$possible == "maybe next time")) {
      clusters <- lapply(clusters, function(clust) {
        clust$possible[clust$possible == "maybe next time"] <- "NA"
        clust
      })
      return(list(possible = FALSE, clusters = clusters)) # pas possible
    }
    return(try_solve_a_cluster(clusters, clust_i, mines_left, grid, void, ...))
  } else if (propagated_game_end[[2]] == "lost") {
    # rares situations (voir tests unitaires) où un mines_trial force un clicker certain de commettre une erreur
    # ex: en pensant qu'il ne reste plus de mines nul part donc qu'on peut cliquer n'importe où
    if (clust_i == length(clusters)) return(list(possible = TRUE, clusters = clusters)) # on a réussi
    if ((clust_i + 1) > length(clusters)) browser()
    return(try_solve_a_cluster(clusters, clust_i + 1, mines_left - trials_order[1], grid, void, ...))
  } else {
    browser() # si propgated_game_end a une fin inattendue
  }
}

