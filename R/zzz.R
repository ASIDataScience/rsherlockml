# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>%
NULL

.onLoad <- function(libname, pkgname) {
  protocol <- Sys.getenv('SHERLOCKML_PROTOCOL')
  domain <- Sys.getenv('SHERLOCKML_DOMAIN')
  options(list(
    sherlockml.hudson_url = paste0(protocol, '://hudson.', domain),
    sherlockml.secret_url = paste0(protocol, '://secret-service.', domain),
    sherlockml.tavern_url = paste0(protocol, '://tavern.', domain)
  ))

  invisible()
}
