# import pipe operators but not all of magrittr
#' @importFrom magrittr %>% %$% %T>% %<>%
NULL

#' Rmarkdown format for a SherlockML report
#'
#' Knits a file to HTML and makes that HTML available as a Sherlock report
#'
#' Simply add \code{output: rsherlock::report} to your Rmd yaml.
#'
#' @param ... all parameters that can be passed to \code{rmarkdown::html_document}
#'
#' @export
report <- function(..., quiet=TRUE, mathjax=NULL) {

  format <- rmarkdown::html_document(..., quiet=quiet, mathjax=mathjax)

  pre_post_processor <- format$post_processor

  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    params <- input_file %>%
      stringr::str_split(pattern='[.]') %>%
      unlist() %>%
      magrittr::extract(1) %>%
      paste0('.Rmd') %>%
      rmarkdown::yaml_front_matter()
    output_file <- pre_post_processor(metadata, input_file, output_file, clean, verbose)

    # edit the HTML to make sure it's all encapsulated in the body tag
    readLines(output_file) %>%
      replace(.=='<body>', '') %>%
      replace(.=='</head>', '') %>%
      replace(.=='<head>', '<body>') %>%
      replace(stringr::str_detect(., '<h1 class="title'), '') %>%
      replace(stringr::str_detect(., '<div class="btn-group pull-right">'),
              '<div class="btn-group pull-right" style="display: none;">')
      writeLines(output_file)

    publish_report(params$title, output_file)

    # make sure HTML file is deleted at the of all operations
    do.call(on.exit, list(substitute(file.remove(output_file)), add = TRUE),
            envir = parent.frame())
    do.call(on.exit, list(substitute(message("\nReport published.")), add = TRUE),
            envir = parent.frame())
    return(output_file)
  }
  return(format)
}


if (!requireNamespace('rsherlock', quietly = T)) {
  placeholder_notebook <- paste0(getwd(), '/empty.ipynb')
} else {
  placeholder_notebook <- system.file("extdata", "empty.ipynb", package="rsherlock")
}


extract_report_info <- function(report_object) {
  return(
    c(magrittr::extract(
        report_object, c("report_name", "report_id", "created_at", "description")),
      magrittr::extract(
        report_object$active_version,
        c("version_id", "report_path", "author_id", "notebook_path", "created_at", "status")))
    )
}


# get a list of report IDs
get_report_list <- function() {
  paste0(getOption('sherlock.tavern_url'), '/project/', getOption('sherlock.project_id')) %>%
    httr::GET(add_hudson_header()) %T>%
    httr::stop_for_status() %>%
    httr::content(as='parsed', type='application/json') %>%
    purrr::map_df(extract_report_info) %>%
    return()
}


publish_report <- function(report_name, report_path) {
  # check if report exists in this project
  print(report_name)
  if (report_name %in% suppressWarnings(get_report_list()$report_name)) {
    message(paste('\nPublishing a new version of existing report named', report_name))
    publish_new_version(report_name, report_path)
  } else {
    message(paste('\nPublishing a new report named', report_name))
    publish_new_report(report_name, report_path)
  }
}


make_tmp_notebook <- function() {
  tmp_notebook <- paste0(tempfile('temp_notebook_', tmpdir = getwd()), '.ipynb')
  file.copy(from = placeholder_notebook, to = tmp_notebook)
  return(tmp_notebook)
}


publish_new_report <- function(report_name, report_path) {

  # copy the empty notebook into the current folder (project scope)
  tmp_notebook <- make_tmp_notebook()
  on.exit(suppressWarnings(file.remove(tmp_notebook)))

  paste0(getOption('sherlock.tavern_url'), '/project/', getOption('sherlock.project_id')) %>%
    httr::POST(body = list(report_name = report_name,
                     notebook_path = substring(tmp_notebook, 10),
                     description = 'This is a description',
                     author_id = getOption('sherlock.user_id')),
         add_hudson_header(),
         encode = 'json') %T>%
    httr::stop_for_status() %>%
    httr::content(as='parsed', type='application/json') ->
    report_object

  wait_and_check(report_object)
  update_report_text(report_path, report_object) %>% return()
}


publish_new_version <- function(name, report_path) {

  # get the right report ID
  # TODO: Maybe get user input for override / new report
  report_id <- get_report_list() %>% dplyr::filter(report_name == name) %$% report_id

  # copy the empty notebook into the current folder (project scope)
  tmp_notebook <- make_tmp_notebook()
  on.exit(suppressWarnings(file.remove(tmp_notebook)))

  paste(getOption('sherlock.tavern_url'), "report", report_id, "version", sep="/") %>%
    httr::POST(body = list(notebook_path = substring(tmp_notebook, 10),
                           author_id = getOption('sherlock.user_id'),
                           draft = F),
               add_hudson_header(),
               encode = 'json') %T>%
    httr::stop_for_status() %>%
    httr::content(as='parsed', type='application/json') ->
    version_object

  report_object <- list(active_version = version_object)
  report_object$report_id <- version_object$report_id

  wait_and_check(report_object)

  # update the HTML
  update_report_text(report_path, report_object) %>% return()
}


wait_and_check <- function(report_object) {

  file_key <- paste0(getOption("sherlock.project_id"), report_object$active_version$report_path)
  sfs_credentials <- get_sfs_credentials()

  repeat {
    Sys.sleep(4)
    if (aws.s3::head_object(object = file_key, bucket = "sml-projects-prod",
                            key = sfs_credentials$access_key,
                            secret = sfs_credentials$secret_key,
                            region = 'eu-west-1', check_region = F)[1]
        & get_report_list() %>%
            dplyr::filter(report_id == report_object$report_id) %$%
            status == "success") { break }
    if (get_report_list() %>%
        dplyr::filter(report_id == report_object$report_id) %$%
        status == "failure") {
      stop("The report publication seems to have failed.")
    }
  }
}


update_report_text <- function(report_path, report_object) {
  sfs_credentials <- get_sfs_credentials()
  file_key <- paste0(getOption("sherlock.project_id"), report_object$active_version$report_path)

  # upload to s3
  aws.s3::put_object(report_path, object = file_key, bucket = "sml-projects-prod",
                     # credentials
                     key = sfs_credentials$access_key,
                     secret = sfs_credentials$secret_key,
                     # encryption
                     headers = c('x-amz-server-side-encryption' = 'AES256'),
                     region = 'eu-west-1', check_region = F)
}
