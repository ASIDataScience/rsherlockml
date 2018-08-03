# Copyright 2018 ASI Data Science

# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>% %<>%
NULL

# the following silences non-standard eval issues w/ CMD CHECK
utils::globalVariables(c("report_id", "report_name", "."))

#' Rmarkdown format for a SherlockML report
#'
#' Knits a file to HTML and makes that HTML available as a SherlockML report
#'
#' Simply add \code{output: rsherlockml::report} to your Rmd yaml.
#'
#' \code{rmarkdown::html_document}
#'
#' @export
report <- function(...) {
  format <- rmarkdown::html_document(..., quiet = TRUE, mathjax = NULL)

  pre_post_processor <- format$post_processor

  format$post_processor <- function( # nocov start
    metadata, input_file, output_file, clean, verbose
  ) {

    params <-
      input_file %>%
      stringr::str_split(pattern = "[.]") %>%
      unlist() %>%
      magrittr::extract(1) %>%
      paste0(".Rmd") %>%
      rmarkdown::yaml_front_matter()

    output_file <-
      pre_post_processor(metadata, input_file, output_file, clean, verbose)

    # edit the HTML to make sure it's all encapsulated in the body tag
    readLines(output_file) %>%
      replace(. == "<body>", "") %>%
      replace(. == "</head>", "") %>%
      replace(. == "<head>", "<body>") %>%
      replace(stringr::str_detect(., '<h1 class="title'), "") %>%
      replace(
        stringr::str_detect(., '<div class="btn-group pull-right">'),
        '<div class="btn-group pull-right" style="display: none;">'
      )
    writeLines(output_file)

    publish_report(params$title, output_file, params$description)

    # make sure HTML file is deleted at the of all operations
    do.call(
      on.exit,
      list(substitute(file.remove(output_file)), add = TRUE),
      envir = parent.frame()
    )

    do.call(
      on.exit,
      list(substitute(message("\nReport published.")), add = TRUE),
      envir = parent.frame()
    )

    return(output_file)
  } # nocov end
  return(format)
}


placeholder_notebook <- system.file(
  "extdata", "empty.ipynb", package = "rsherlockml"
)


extract_report_info <- function(report_object) {
  return(
    c(
      magrittr::extract(
        report_object,
        c("report_name", "report_id", "created_at", "description")
      ),
      magrittr::extract(
        report_object$active_version,
        c("version_id", "report_path", "author_id",
          "notebook_path", "created_at", "status")
      )
    )
  )
}


# get a list of report IDs
get_report_list <- function() {
  set_hudson_token()
  set_user_id()

  getOption("sherlockml.tavern_url") %>%
    paste("project", Sys.getenv("SHERLOCKML_PROJECT_ID"), sep = "/") %>%
    httr::GET(add_hudson_header()) %T>%
    httr::stop_for_status() %>%
    httr::content(as = "parsed", type = "application/json") %>%
    purrr::map_df(extract_report_info) %>%
    return()
}


publish_report <- function(report_name, report_path, description) {

  # set some required options
  set_hudson_token()
  set_user_id()

  # check if report exists in this project

  if (report_name %in% suppressWarnings(get_report_list()$report_name)) {
    message(paste(
      "\nPublishing a new version of existing report named",
      report_name
    ))
    publish_new_version(report_name, report_path)
  } else {
    message(paste("\nPublishing a new report named", report_name))
    publish_new_report(report_name, report_path, description)
  }
}


make_tmp_notebook <- function() {
  project_dir <- function() "/project"  # nolint
  tmp_notebook <- paste0(
    tempfile(".temp_notebook_", tmpdir = project_dir()), ".ipynb"
  )
  file.copy(from = placeholder_notebook, to = tmp_notebook)
  return(tmp_notebook)
}


publish_new_report <- function(
  report_name,
  report_path,
  description
) {
  # copy the empty notebook into the current folder (project scope)
  tmp_notebook <- make_tmp_notebook()
  on.exit(suppressWarnings(file.remove(tmp_notebook)))

  body <- list(
    report_name = report_name,
    notebook_path = substring(tmp_notebook, 10),
    description = description,
    author_id = getOption("sherlockml.user_id")
  )
  print(body)

  getOption("sherlockml.tavern_url") %>%
    paste("project", Sys.getenv("SHERLOCKML_PROJECT_ID"), sep = "/") %>%
    httr::POST(
      body = body,
      add_hudson_header(),
      encode = "json"
    ) %T>%
    httr::stop_for_status() %>%
    httr::content(as = "parsed", type = "application/json") ->
  report_object

  wait_and_check(report_object)
  update_report_text(report_path, report_object) %>%
    return()
}


publish_new_version <- function(name, report_path) {

  # get the right report ID
  # TODO: Maybe get user input for override / new report
  report_id <-
    get_report_list() %>%
    dplyr::filter(report_name == name) %>%
    magrittr::extract2('report_id')

  # copy the empty notebook into the current folder
  tmp_notebook <- make_tmp_notebook()
  on.exit(suppressWarnings(file.remove(tmp_notebook)))

  version_object <-
    getOption("sherlockml.tavern_url") %>%
    paste("report", report_id, "version", sep = "/") %>%
    httr::POST(
      body = list(notebook_path = substring(tmp_notebook, 10),
                  author_id = getOption("sherlockml.user_id"),
                  draft = FALSE),
      add_hudson_header(),
      encode = "json"
    ) %T>%
    httr::stop_for_status() %>%
    httr::content(as = "parsed", type = "application/json")

  report_object <- list(active_version = version_object)
  report_object$report_id <- version_object$report_id

  wait_and_check(report_object)

  # update the HTML
  update_report_text(report_path, report_object) %>%
    return()
}


wait_and_check <- function(report_object) {

  sfs_credentials <- get_sfs_credentials()

  repeat {

    filtered_reports <-
      get_report_list() %>%
      dplyr::filter(report_id == report_object$report_id)

    if (filtered_reports$status == "failure") {
      stop("The report publication seems to have failed.")
    }

    Sys.sleep(4)

    if (
      aws.s3::head_object(
        object = report_object$active_version$report_key,
        bucket = report_object$active_version$report_bucket,
        key = sfs_credentials$access_key,
        secret = sfs_credentials$secret_key,
        region = "eu-west-1", check_region = FALSE
      ) %>% utils::head(1)
      && filtered_reports$status == "success"
    ) {
      break
    }
  }
}


update_report_text <- function(report_path, report_object) {

  sfs_credentials <- get_sfs_credentials()

  file_key <- paste0(
    Sys.getenv("SHERLOCKML_PROJECT_ID"),
    report_object$active_version$report_path
  )

  # upload to s3
  aws.s3::put_object(
    report_path,
    object = file_key, bucket = sfs_credentials$bucket,
    # credentials
    key = sfs_credentials$access_key,
    secret = sfs_credentials$secret_key,
    # encryption
    headers = c("x-amz-server-side-encryption" = "AES256"),
    region = "eu-west-1", check_region = FALSE
  )
}
