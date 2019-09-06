# Copyright 2018-2019 Faculty Science Limited

# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>%
NULL

utils::globalVariables(
  c("account", "expires_in", "access_token", "token_type",
    "userId")
)

set_hudson_token <- function() {
  # check if token is already set
  if (!("faculty.hudson.expiry" %in% names(options()))
      || (getOption("faculty.hudson.expiry") < lubridate::now())) {

    getOption("faculty.hudson_url") %>%
      paste("access_token", sep = "/") %>%
      httr::POST(body = list(
        client_id = Sys.getenv("FACULTY_CLIENT_ID"),
        client_secret = Sys.getenv("FACULTY_CLIENT_SECRET"),
        grant_type = "client_credentials"
      ), encode = "json") %>%
      httr::content(as = "parsed", type = "application/json") %$%
      options(list(
        faculty.hudson.expiry =
          lubridate::now() + lubridate::seconds(expires_in),
        faculty.hudson.token =
          paste(token_type, access_token)
      ))

  }

  NULL
}

set_user_id <- function() {
  if (!("faculty.user_id" %in% names(options()))) {

    getOption("faculty.hudson_url") %>%
      paste("authenticate", sep = "/") %>%
      httr::GET(httr::add_headers(
        Authorization = getOption("faculty.hudson.token")
      )) %>%
      httr::content(as = "parsed", type = "application/json") %$%
      account %$%
      options(list(faculty.user_id = userId))
  }
  NULL
}

add_hudson_header <- function() {
  httr::add_headers(Authorization = getOption("faculty.hudson.token"))
}
