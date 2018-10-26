# Copyright 2018 ASI Data Science

# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>%
NULL

utils::globalVariables(
  c("account", "expires_in", "access_token", "token_type",
    "userId")
)

set_hudson_token <- function() {
  # check if token is already set
  if (!("sherlockml.hudson.expiry" %in% names(options()))
      || (getOption("sherlockml.hudson.expiry") < lubridate::now())) {

    getOption("sherlockml.hudson_url") %>%
      paste("access_token", sep = "/") %>%
      httr::POST(body = list(
        client_id = Sys.getenv("SHERLOCKML_CLIENT_ID"),
        client_secret = Sys.getenv("SHERLOCKML_CLIENT_SECRET"),
        grant_type = "client_credentials"
      ), encode = "json") %>%
      httr::content(as = "parsed", type = "application/json") %$%
      options(list(
        sherlockml.hudson.expiry =
          lubridate::now() + lubridate::seconds(expires_in),
        sherlockml.hudson.token =
          paste(token_type, access_token)
      ))

  }

  NULL
}

set_user_id <- function() {
  if (!("sherlockml.user_id" %in% names(options()))) {

    getOption("sherlockml.hudson_url") %>%
      paste("authenticate", sep = "/") %>%
      httr::GET(httr::add_headers(
        Authorization = getOption("sherlockml.hudson.token")
      )) %>%
      httr::content(as = "parsed", type = "application/json") %$%
      account %$%
      options(list(sherlockml.user_id = userId))
  }
  NULL
}

add_hudson_header <- function() {
  httr::add_headers(Authorization = getOption("sherlockml.hudson.token"))
}

get_datasets_credentials <- function() {
  set_hudson_token()

  getOption("sherlockml.secret_url") %>%
  paste("sfs", Sys.getenv("SHERLOCKML_PROJECT_ID"), sep = "/") %>%
    httr::GET(httr::add_headers(
      Authorization = getOption("sherlockml.hudson.token")
    )) %>%
    httr::content(as = "parsed", type = "application/json") ->
    datasets_credentials

  if (!is.null(datasets_credentials) && datasets_credentials$verified) {
      return(datasets_credentials)
  } else {
      Sys.sleep(5)
      return(get_datasets_credentials())
  }
}
