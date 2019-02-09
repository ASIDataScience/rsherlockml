# Copyright 2018-2019 Faculty Science Limited

# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>%
NULL

# utils::globalVariables(c("pydatasets"))
pydatasets <- NULL

.onLoad <- function(libname, pkgname) {
  protocol <- Sys.getenv("FACULTY_PROTOCOL")
  domain <- Sys.getenv("FACULTY_DOMAIN")
  options(list(
    faculty.hudson_url = paste0(protocol, "://hudson.", domain),
    faculty.secret_url = paste0(protocol, "://secret-service.", domain),
    faculty.tavern_url = paste0(protocol, "://tavern.", domain)
  ))

  pydatasets <<- reticulate::import("faculty.datasets", delay_load = TRUE)  # nolint
  NULL
}
