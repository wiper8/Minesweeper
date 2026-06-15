library(testthat)
library(here)
library(mockery)

path <- "tests/testthat"

# test_dir(path)

# overwrite browser function to throw error
browser <- function(...) stop("browser() was called", call. = FALSE)

sapply(
  setdiff(list.files(path), c()),
  function(file) {
    print(file)
    source(file.path(path, file))
  }
)

rm(browser)
