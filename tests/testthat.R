library(testthat)
library(here)
library(mockery)

path <- "tests/testthat"

# test_dir(path)

sapply(
  setdiff(list.files(path), c("test_can_deduce_pattern.R", "test_independant_clusters.R")),
  function(file) {
    print(file)
    source(file.path(path, file))
  }
)
