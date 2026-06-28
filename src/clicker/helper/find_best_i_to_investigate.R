source("src/indicies/i_and_positions.R")
source("src/indicies/get_around_square.R")

find_best_i_to_investigate <- function(grid, solved_around, click_order) {
  priorities_i <- priority_investigate(grid, solved_around)
  
  if (is.null(click_order)) {
    i_to_investigate <- which(grid > 0 & solved_around == 0)
  } else {
    dims <- dim(grid)
    i_to_investigate <- position_to_i_mat(click_order[rev(seq_len(nrow(click_order))), , drop = FALSE], dims)
    # s'assurer de juste investiguer les cases pertinentes
    i_to_investigate_filtered <- i_to_investigate[grid[i_to_investigate] > 0 & solved_around[i_to_investigate] == 0]
    # au cas où on en oubli, quand des cases sont révélées automatiquement sans avoir été cliquées
    to_union <- which(grid > 0 & solved_around == 0)
    # et les ajouter en ordre de proximité au dernier clicked
    to_union <- to_union[order(sapply(to_union, function(i) {
      coordinates <- i_to_position(i, dims = dims)
      # distance de manhattan
      sum(abs(coordinates - i_to_position(i_to_investigate[1], dims)))
    }))]
    i_to_investigate <- union(
      i_to_investigate_filtered,
      to_union
    )
  }
  unique(c(priorities_i, i_to_investigate))
}


priority_investigate <- function(grid, solved_around) {
  seuil_priorite <- 0.15
  dims <- dim(grid)
  mines_left_around_grid <- grid * NA
  n_unknown_grid <- grid * NA
  for (i in which(grid > 0 & solved_around < 1)) {
    if (solved_around[i] < 1) {
      pos <- i_to_position(i, dims)
      values <- get_around_square(pos, grid, dims)
      unknown <- !values %in% known
      
      n_unknown_grid[i] <- sum(unknown)
      n_mines_left_around <- grid[i] - sum(values %in% hp_flags)
      mines_left_around_grid[i] <- n_mines_left_around
    }
  }
  
  # trier selon la différence entre voisins : 
  # lorsqu'une case a plusieurs mines restantes et peu de cases, ET
  # qu'un de ses voisins a peu de mines et beaucoup de cases,
  # c'est très prometteur

  ratio_grid <- mines_left_around_grid / n_unknown_grid
  to_priorise <- numeric(sum(!is.na(ratio_grid)))
  i <- which(!is.na(ratio_grid))

  for (i_itr in seq_along(i)) {
    pos <- i_to_position(i[i_itr], dims)
    values <- get_around_cross(pos, ratio_grid, dims)
    to_priorise[i_itr] <- suppressWarnings(
      max(
        abs(values[-1, , drop = FALSE] - values[-nrow(values), , drop = FALSE]),
        abs(values[, -1, drop = FALSE] - values[, -ncol(values), drop = FALSE]),
        na.rm = TRUE
      )
    )
    if (to_priorise[i_itr] < seuil_priorite) to_priorise[i_itr] <- NA
  }
  
  first_order <- order(to_priorise, decreasing = TRUE)
  first_order <- first_order[seq_len(sum(!is.na(to_priorise)))]
  i[first_order]
}
