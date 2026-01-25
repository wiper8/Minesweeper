library(testthat)
library(here)
library(mockery)

path <- "tests/testthat"

test_dir(path)

sapply(list.files(path), function(file) test_file(file.path(path, file)))

sapply(list.files(path), function(file) source(file.path(path, file)))
