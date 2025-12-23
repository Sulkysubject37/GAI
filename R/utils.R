# =========================================================
# utils.R
# Shared utility functions for GAI application and scripts
# =========================================================

#' Read a delimited file with auto-detection of delimiter
#'
#' @param path Path to the input file
#' @return A data frame
read_any_delim <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  # Read first line to detect delimiter
  first_line <- readr::read_lines(path, n_max = 1)
  
  if (length(first_line) == 0) {
    stop("File is empty: ", path)
  }
  
  n_tabs <- stringr::str_count(first_line, "\t")
  n_commas <- stringr::str_count(first_line, ",")
  
  delim <- if (n_tabs > n_commas) "\t" else ","
  
  # Read with detected delimiter, forcing all columns to character initially for safety
  readr::read_delim(path, delim = delim, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
}

#' Ensure directory exists
#' 
#' @param path Path to directory
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

