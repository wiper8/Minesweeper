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
