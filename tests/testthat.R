# Copyright 2018-2019 Faculty Science Limited

Sys.setenv(RETICULATE_PYTHON = "~/.venv/r-reticulate/bin/python")  # nolint

library(testthat)
library(assertthat)
library(mockery)
library(lubridate)
library(rfaculty)

test_check("rfaculty")
