# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>%
NULL

.onLoad <- function(libname, pkgname) {

  options(list(
    sherlockml.hudson_url = "https://hudson.platform.asidata.science",
    sherlockml.secret_url = "https://secret-service.platform.asidata.science",
    sherlockml.tavern_url = "https://tavern.platform.asidata.science"
  ))

  invisible()
}
