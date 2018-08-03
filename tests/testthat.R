Sys.setenv(RETICULATE_PYTHON = "~/.venv/r-reticulate/bin/python")  # nolint

library(testthat)
library(assertthat)
library(mockery)
library(lubridate)
library(rsherlockml)

test_check("rsherlockml")
