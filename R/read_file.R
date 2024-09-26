#' Record Input and Output
#'
#' This function parses R scripts and records input and output information.
#'
#' @param file_path Path to the R script
#' @return A list containing input and output information
#' @export
record_io <- function(file_path) {
  lines <- readLines(file_path)
  inputs <- character()
  outputs <- character()
  
  # Define file extensions to look for
  file_extensions <- c("csv", "txt", "Rds", "png", "jpg", "pdf")
  extension_pattern <- paste0("\\.(", paste(file_extensions, collapse = "|"), ")")
  
  for (i in seq_along(lines)) {
    if (grepl("##'\\s*input", lines[i], ignore.case = TRUE)) {
      # Extract file paths in quotes that match the extension pattern
      matches <- stringr::str_extract_all(lines[i + 1], paste0('"([^"]*', extension_pattern, ')"'))
      if (length(matches[[1]]) > 0) {
        inputs <- c(inputs, gsub('"', '', matches[[1]]))
      }
    } else if (grepl("##\\s*output", lines[i], ignore.case = TRUE)) {
      # Extract file paths in quotes that match the extension pattern
      matches <- stringr::str_extract_all(lines[i + 1], paste0('"([^"]*', extension_pattern, ')"'))
      if (length(matches[[1]]) > 0) {
        outputs <- c(outputs, gsub('"', '', matches[[1]]))
      }
    }
  }
  
  list(inputs = inputs, outputs = outputs, script = file_path)
}