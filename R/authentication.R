# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>%
NULL

set_hudson_token <- function() {
  # check if token is already set
  if (!("sherlockml.hudson.expiry" %in% names(options()))
      || (getOption("sherlockml.hudson.expiry") < lubridate::now())) {

    paste0(getOption("sherlockml.hudson_url"), "/access_token") %>%
      httr::POST(body = list(client_id = Sys.getenv("SHERLOCKML_CLIENT_ID"),
                             client_secret = Sys.getenv("SHERLOCKML_CLIENT_SECRET"),
                             grant_type = "client_credentials"),
                 encode = "json") %>%
      httr::content(as = "parsed", type = "application/json") %$%
      options(list(
        sherlockml.hudson.expiry = lubridate::now() + lubridate::seconds(expires_in),
        sherlockml.hudson.token = paste(token_type, access_token)
      ))

  }
  NULL
}

set_user_id <- function() {
  if (!('sherlockml.user_id' %in% names(options()))) {
    getOption('sherlockml.hudson_url') %>%
      paste0('/authenticate') %>%
      httr::GET(httr::add_headers(Authorization = getOption('sherlockml.hudson.token'))) %>%
      httr::content(as='parsed', type='application/json') %$%
      account %$%
      options(list(sherlockml.user_id = userId))
  }
  NULL
}

get_access_object <- function(source = "environment") {
  set_hudson_token()
  set_user_id()
  # get auth headers from environment
  client_id <- Sys.getenv('SHERLOCKML_CLIENT_ID')
  client_secret <- Sys.getenv('SHERLOCKML_CLIENT_SECRET')
  # make the request
  access_object <- paste0(getOption("sherlockml.hudson_url"), "/access_token") %>%
    httr::POST(body = list(client_id = client_id, client_secret = client_secret,
                           grant_type = "client_credentials"), encode = "json") %>%
    httr::content(as = "parsed", type = "application/json")
  return(access_object)
}

add_hudson_header <- function() {
  return(httr::add_headers(Authorization = getOption("sherlockml.hudson.token")))
}

get_sfs_credentials <- function() {
  set_hudson_token()
  sfs_credentials <- paste0(getOption("sherlockml.secret_url"), "/sfs/", Sys.getenv("SHERLOCKML_PROJECT_ID")) %>%
    httr::GET(httr::add_headers(Authorization = getOption('sherlockml.hudson.token'))) %>%
    httr::content(as = "parsed", type = "application/json")
  if (!is.null(sfs_credentials) && sfs_credentials$verified) {
      return(sfs_credentials)
  } else {
      Sys.sleep(5)
      return(get_sfs_credentials())
  }
}
