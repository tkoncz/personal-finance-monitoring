source("libraries.R")
invisible(sapply(list.files("R", full.names = TRUE), source))

library(testthat)
library(mockery)

test_dir("tests/testthat")
# test_dir('tests', reporter = "summary", stop_on_failure = TRUE)
