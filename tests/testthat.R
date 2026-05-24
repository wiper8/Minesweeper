library(testthat)
library(here)
library(mockery)

path <- "tests/testthat"

# test_dir(path)

sapply(
  setdiff(list.files(path), c("test_6_compute_mine_probability.R", "test_7_compute_grid_probabilities.R")),
  function(file) {
    print(file)
    source(file.path(path, file))
  }
)
