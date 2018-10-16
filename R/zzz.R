# Copyright 2018 ASI Data Science

# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>%
NULL

# utils::globalVariables(c("pydatasets"))
pydatasets <- NULL

.onLoad <- function(libname, pkgname) {
  protocol <- Sys.getenv("SHERLOCKML_PROTOCOL")
  domain <- Sys.getenv("SHERLOCKML_DOMAIN")
  options(list(
    sherlockml.hudson_url = paste0(protocol, "://hudson.", domain),
    sherlockml.secret_url = paste0(protocol, "://secret-service.", domain),
    sherlockml.tavern_url = paste0(protocol, "://tavern.", domain)
  ))

  pydatasets <<- reticulate::import("sherlockml.datasets", delay_load = TRUE)  # nolint
  NULL
}
