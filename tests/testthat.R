library(testthat)
library(here)

path <- "tests/testthat"

test_dir(path)

sapply(list.files(path), function(file) source(file.path(path, file)))
