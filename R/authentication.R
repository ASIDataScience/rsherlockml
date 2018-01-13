# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>%
NULL

get_access_object <- function(source = "environment") {
  # get auth headers from environment
  client_id <- getOption("sherlock.client_id")
  client_secret <- getOption("sherlock.client_secret")
  # make the request
  access_object <- paste0(getOption("sherlock.hudson_url"), "/access_token") %>%
    httr::POST(body = list(client_id = client_id, client_secret = client_secret,
                           grant_type = "client_credentials"), encode = "json") %>%
    httr::content(as = "parsed", type = "application/json")
  return(access_object)
}

add_hudson_header <- function() {
  access_object <- get_access_object()
  return(httr::add_headers(Authorization = paste(access_object$token_type, access_object$access_token)))
}

get_sfs_credentials <- function() {
    access_object <- get_access_object()
    sfs_credentials <- paste0(getOption("sherlock.secret_url"), "/sfs/", getOption("sherlock.project_id")) %>%
      httr::GET(
        httr::add_headers(Authorization = paste(access_object$token_type,
                                                access_object$access_token))) %>%
      httr::content(as = "parsed", type = "application/json")
    if (sfs_credentials$verified) {
        return(sfs_credentials)
    } else {
        Sys.sleep(5)
        return(get_sfs_credentials())
    }
}
