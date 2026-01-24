library(testthat)
library(here)
library(mockery)

path <- "tests/testthat"

test_dir(path)

sapply(list.files(path), function(file) source(file.path(path, file)))
