# Copyright 2018-2019 Faculty Science Limited

#' Copy from Faculty datasets to the local filesystem.
#'
#' @param datasets_path File path on Faculty datasets
#' @param local_path The destination path on the local filesystem.
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_get <-
  function(datasets_path, local_path, project_id = NULL) {
    pydatasets$get(datasets_path, local_path, project_id)
  }

#' Copy from the local filesystem to Faculty datasets.
#'
#' @param local_path The destination path on the local filesystem.
#' @param datasets_path File path on Faculty datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_put <-
  function(local_path, datasets_path, project_id = NULL) {
    pydatasets$put(local_path, datasets_path, project_id)
  }

#' List files on Faculty datasets.
#'
#' @param prefix The prefix by which to filter files (default: '/')
#' @param project_id The ID of the project; by default, the current project is used.
#' @param show_hidden Whether to show hidden files (files prefixed with a dot).
#' @export
datasets_list <-
  function(prefix = "/",
           project_id = NULL,
           show_hidden = FALSE) {
    pydatasets$ls(prefix, project_id, show_hidden)
  }

#' Copy a file from one location to another on Faculty Datasets.
#'
#' @param source_path Source path on Faculty datasets
#' @param destination_path Destination path on Faculty datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_copy <-
  function(source_path,
           destination_path,
           project_id = NULL)	 {
    pydatasets$cp(source_path, destination_path, project_id)
  }

#' Move a file from one location to another on Faculty Datasets.
#'
#' @param source_path Source path on Faculty datasets
#' @param destination_path Destination path on Faculty datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_move <-
  function(source_path,
           destination_path,
           project_id = NULL) {
    pydatasets$mv(source_path, destination_path, project_id)
  }

#' Delete a file from Faculty Datasets.
#'
#' @param path File path on Faculty datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_delete <- function(path, project_id = NULL) {
  pydatasets$rm(path, project_id)
}

#' Retrieve the etag for a file on Faculty datasets.
#'
#' @param path File path on Faculty datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_etag <- function(path, project_id = NULL) {
  pydatasets$etag(path, project_id)
}
