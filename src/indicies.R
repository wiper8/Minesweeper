get_around_square <- function(pos, grid) {
  dims <- dim(grid)
  
  x <- pos[1] + -1:1
  y <- pos[2] + -1:1
  x <- x[x > 0 & x <= dims[1]]
  y <- y[y > 0 & y <= dims[2]]
  grid[x, y]
}

square_pos <- function(pos, grid) {
  dims <- dim(grid)
  
  x <- pos[1] + -1:1
  y <- pos[2] + -1:1
  x <- x[x > 0 & x <= dims[1]]
  y <- y[y > 0 & y <= dims[2]]
  matrix(c(
    rep(x, length(y)),
    rep(y, each = length(x))
  ), ncol = 2)
}

position_to_i <- function(pos, dims) {
  (pos[2] - 1) * dims[1] + pos[1]
}
position_to_i_mat <- function(pos, dims) {
  (pos[, 2] - 1) * dims[1] + pos[, 1]
}

i_to_position <- function(i, dims) {
  col <- floor(i / dims[1])
  col <- col + (i / dims[1] != floor(i / dims[1]))
  c(i - (col - 1) * dims[1], col)
}
