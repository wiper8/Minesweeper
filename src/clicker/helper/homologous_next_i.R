homologous_next_i <- function(grid, next_i, mines_left, solved_around, in_cluster, dims = dim(grid)) {
  pos <- i_to_position(next_i, dims)
  tmp <- square_pos_and_get_around_square_big(pos, grid, dims)
  homologous_candidates <- tmp[[1]]
  # retirer les cas connus et les cas hors cluster et next_i
  keep <- apply(homologous_candidates, 1, function(pos_x) !grid[pos_x[1], pos_x[2]] %in% known && in_cluster[pos_x[1], pos_x[2]] && !all(pos_x == pos))
  homologous_candidates <- homologous_candidates[keep, , drop = FALSE]
  if (nrow(homologous_candidates) > 0) {
    # filtrer pour conserver les mêmes voisins influants
    influence <- tmp[[1]][tmp[[2]] >= 0, , drop = FALSE]
    if (nrow(influence) == 0) return(c())
    
    homologous_candidates <- homologous_candidates[
      apply(homologous_candidates, 1, function(pos_x) {
        tmp <- square_pos_and_get_around_square(pos_x, grid, dims)
        influence_x <- tmp[[1]][tmp[[2]] >= 0, , drop = FALSE]
        isTRUE(all.equal(influence, influence_x))
      }),
      ,
      drop = FALSE
    ]
    return(position_to_i_mat(homologous_candidates, dims))
  } else {
    c()
  }
}
