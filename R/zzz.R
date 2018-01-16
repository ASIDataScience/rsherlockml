# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>%
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()

  op.rsherlock <- list(
    sherlock.hudson_url = "https://hudson.platform.asidata.science",
    sherlock.secret_url = "https://secret-service.platform.asidata.science",
    sherlock.tavern_url = "https://tavern.platform.asidata.science",
    sherlock.project_id = Sys.getenv("SHERLOCKML_PROJECT_ID"),
    sherlock.client_id = Sys.getenv("SHERLOCKML_CLIENT_ID"),
    sherlock.client_secret = Sys.getenv("SHERLOCKML_CLIENT_SECRET")
  )
  toset <- !(names(op.rsherlock) %in% names(op))
  if(any(toset)) options(op.rsherlock[toset])

  # also set the user_id
  header_string <- paste0(getOption("sherlock.hudson_url"), "/access_token") %>%
    httr::POST(body = list(client_id = getOption("sherlock.client_id"),
                           client_secret = getOption("sherlock.client_secret"),
                           grant_type = "client_credentials"), encode = "json") %>%
    httr::content(as = "parsed", type = "application/json") %$%
    paste(token_type, access_token)

  user_id <- getOption('sherlock.hudson_url') %>%
    paste0('/authenticate') %>%
    httr::GET(httr::add_headers(Authorization = header_string)) %>%
    httr::content(as='parsed', type='application/json') %$%
    account %$%
    userId

  options(list(sherlock.user_id = user_id))

  invisible()
}
