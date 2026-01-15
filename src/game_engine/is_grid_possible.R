is_grid_possible <- function(grid) {
  
  for (i in which(grid >= 0)) {
    pos <- i_to_position(i, dim(grid))
    square <- get_around_square(pos, grid)
    if (grid[i] > sum(square %in% c(covered_no_mine, covered_mine, flag_on_mine, flag_on_no_mine))) return(FALSE)
    if (grid[i] < sum(square %in% c(flag_on_mine, flag_on_no_mine))) return(FALSE)
  }
  TRUE
}
