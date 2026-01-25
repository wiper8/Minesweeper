source("src/indicies/count.R")

certain_core <- function(grid, total_mines, solved_around) {
  tmp <- can_flag_all_around(grid, solved_around)
  if (!is.null(tmp)) return(tmp)
  tmp <- can_click_all_around(grid, solved_around)
  if (!is.null(tmp)) return(tmp)
  # tmp <- can_deduce_pattern()
  # if (!is.null(tmp)) return(tmp)
  # tmp <- can_deduce_pattern_knowing_mines_left()
  # if (!is.null(tmp)) return(tmp)
  
  NULL # retourner NULL si on ne sait pas quelle action certain prendre.
}

can_flag_all_around <- function(grid, solved_around) {
  for (i in which(grid > 0 & solved_around == 0)) {
    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    n_unknown <- count_unknown(grid, i, values)
    if (n_unknown > 0 && count_mines_left_around(grid, i, values) == n_unknown) {
      unknown <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine)
      return(list(positions[unknown, , drop = FALSE][1, ], FALSE))
    }
  }
  NULL
}

can_click_all_around <- function(grid, solved_around) {
  for (i in which(grid > 0 & solved_around == 0)) {
    tmp <- count_core(grid, i)
    values <- tmp$values
    positions <- tmp$positions
    n_unknown <- count_unknown(grid, i, values)
    if (n_unknown > 0 && count_mines_left_around(grid, i, values) == 0) {
      unknown <- !values %in% c(0:9, flag_on_mine, flag_on_no_mine)
      return(list(positions[unknown, , drop = FALSE][1, ], TRUE))
    }
  }
  NULL
}
