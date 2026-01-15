sanity_check <- function(grid, status = NULL) {
  if (is.null(status)) {
    status <- matrix(1, nrow(grid), ncol(grid))
  }
  
  for (next_i in which(grid > 0 & status)) {
    pos <- i_to_position(next_i, dim(grid))
    values <- get_around_square(pos, grid)
    unclicked <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine, uncovered_unknown)
    unknown <- unclicked & values != flag_on_mine
    n_unknown <- sum(unknown)
    if (n_unknown == 0) next
    mines_left <- (grid[next_i] - sum(values %in% c(flag_on_mine, flag_on_no_mine)))
    if (mines_left == 0) next
    if (mines_left < 0) {
      return(FALSE)
    }
  }
  TRUE
}
