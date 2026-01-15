compute_box_number <- function(square, human = FALSE) {
  sum(square %in% c(covered_mine, flag_on_mine) | (square == flag_on_no_mine & human))
}
