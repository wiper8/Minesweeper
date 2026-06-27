source("src/indicies/square_pos.R")
source("src/indicies/get_around_square.R")

square_pos_and_get_around_square <- function(pos, grid, dims = dim(grid)) {
  x <- pos[1] + -1:1
  y <- pos[2] + -1:1
  x <- x[x > 0 & x <= dims[1]]
  y <- y[y > 0 & y <= dims[2]]
  
  
  list(
    matrix(
      c(
        rep(x, length(y)),
        rep(y, each = length(x))
      ),
      ncol = 2
    ),
    grid[x, y]
  )
}

square_pos_and_get_around_square_big <- function(pos, grid, dims = dim(grid)) {
  x <- pos[1] + -2:2
  y <- pos[2] + -2:2
  x <- x[x > 0 & x <= dims[1]]
  y <- y[y > 0 & y <= dims[2]]
  
  
  list(
    matrix(
      c(
        rep(x, length(y)),
        rep(y, each = length(x))
      ),
      ncol = 2
    ),
    grid[x, y]
  )
}
