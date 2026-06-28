source("src/game_engine/convert_grid_solution_to_human_grid.R")

is_cluster_island <- function(grid, clusters) {
  dims <- dim(grid)
  grid_tmp <- convert_grid_solution_to_human_grid(grid)

  potential_i_island <- which(!clusters$void$in_cluster & grid_tmp == -10)

  if (length(potential_i_island) == 0) return(rep(FALSE, length(probs_grd_lst$clusters)))

  sapply(clusters$clusters, function(lst) {
    potential_i_island <- which(lst$in_cluster & !lst$grid %in% known)
    for (i in seq_along(potential_i_island)) {
      if (!all(get_around_square(
        i_to_position(potential_i_island[i], dims),
        lst$in_cluster,
        dims
      ))) return(FALSE)
    }
    TRUE
  })
}
