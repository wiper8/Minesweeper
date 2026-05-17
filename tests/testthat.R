library(testthat)
library(here)
library(mockery)

path <- "tests/testthat"

# test_dir(path)

sapply(
  setdiff(list.files(path), c()),
  function(file) {
    print(file)
    source(file.path(path, file))
  }
)
