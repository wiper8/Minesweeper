library(testthat)
library(here)
library(mockery)

path <- "tests/testthat"

# test_dir(path)

sapply(
  setdiff(list.files(path), c("test_certain_core.R", "test_is_mine_propagation_possible.R")),
  function(file) {
    print(file)
    source(file.path(path, file))
  }
)
