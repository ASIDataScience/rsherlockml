# Copyright 2018 ASI Data Science

#' Copy from the SherlockML datasets to the local filesystem.
#'
#' @param datasets_path File path on SherlockML datasets
#' @param local_path The destination path on the local filesystem.
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_get <-
  function(datasets_path, local_path, project_id = NULL) {
    pysfs$get(datasets_path, local_path, project_id)
  }

#' Copy from the local filesystem to SherlockML datasets.
#'
#' @param local_path The destination path on the local filesystem.
#' @param datasets_path File path on SherlockML datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_put <-
  function(local_path, datasets_path, project_id = NULL) {
    pysfs$put(local_path, datasets_path, project_id)
  }

#' List files on SherlockML datasets.
#'
#' @param prefix The prefix by which to filter files (default: '/')
#' @param project_id The ID of the project; by default, the current project is used.
#' @param show_hidden Whether to show hidden files (files prefixed with a dot).
#' @export
datasets_list <-
  function(prefix = "/",
           project_id = NULL,
           show_hidden = FALSE) {
    pysfs$ls(prefix, project_id, show_hidden)
  }

#' Copy a file from one location to another on SherlockML Datasets.
#'
#' @param source_path Source path on SherlockML datasets
#' @param destination_path Destination path on SherlockML datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_copy <-
  function(source_path,
           destination_path,
           project_id = NULL)	 {
    pysfs$cp(source_path, destination_path, project_id)
  }

#' Move a file from one location to another on SherlockML Datasets.
#'
#' @param source_path Source path on SherlockML datasets
#' @param destination_path Destination path on SherlockML datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_move <-
  function(source_path,
           destination_path,
           project_id = NULL) {
    pysfs$mv(source_path, destination_path, project_id)
  }

#' Delete a file from SherlockML Datasets.
#'
#' @param path File path on SherlockML datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_delete <- function(path, project_id = NULL) {
  pysfs$rm(path, project_id)
}

#' Retrieve the etag for a file on SherlockML datasets.
#'
#' @param path File path on SherlockML datasets
#' @param project_id The ID of the project; by default, the current project is used.
#' @export
datasets_etag <- function(path, project_id = NULL) {
  pysfs$etag(path, project_id)
}
