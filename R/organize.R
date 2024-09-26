#' Organize Workflow Information
#'
#' This function collects all recorded workflow information within a project.
#'
#' @param path Path to the project directory
#' @return A list of workflow information for each R script
#' @export
organize <- function(path) {
  r_files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  workflow_info <- lapply(r_files, record_io)
  names(workflow_info) <- basename(r_files)
  workflow_info
}