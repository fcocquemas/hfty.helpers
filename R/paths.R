# Helpers for working with file names, directories, etc.

# Functions:
#   file_path(...)
#   extension(path, dot = TRUE)
#   add_extension(path, ext)
#   remove_extension(path)
#   update_extension(path, ext)
#   basename(path, remove.extensions = FALSE)
#   dirname(path, remove.scheme = FALSE)
#   url_scheme(path, colonslashes = TRUE)

#' Extends the file.path function to avoid doubled trailing slashes
#' unless preceded by colon (for protocol such as http://)
#'
#' @param ... character vectors passed to file.path after cleaning
#'
#' @examples
#' file_path("dir1/dir2//", "dir3", "file.ext")
#'
#' @export
file_path <- function(...) {
  do.call('file.path', lapply(list(...), stringr::str_replace, '(?<!:)\\/+$', ''))
}


#' Get file extension from file name
#'
#' @param path string or vector of strings of file names
#' @param dot logical whether to include dot with extension (defaults to TRUE)
#'   No dot if extension is missing
#'
#' @return string or vector of string of file extensions
#'
#' @export
extension <- function(path, dot = TRUE) {
  path <- tools::file_ext(path)
  paste0(ifelse(dot & path != "", ".", ""), path)
}


#' Adds file extension to file names
#'
#' @param path string or vector of strings of file names
#' @param ext string new extension, with or without the dot
#'
#' @return string or vector of strings of file names without extensions
#'
#' @export
add_extension <- function(path, ext) {
  sapply(path, USE.NAMES = FALSE, paste0, ".", gsub("^\\.", "", ext))
}


#' Removes file extension, keeping the path if provided
#'
#' @param path string or vector of strings of file names
#'
#' @return string or vector of strings of file names without extensions
#'
#' @export
remove_extension <- function(path) {
  sapply(path, USE.NAMES = FALSE, function(x)
    gsub(paste0("\\", extension(x), "$"), "", x))
}


#' Change file extension, or add if it is missing
#'
#' @param path string or vector of strings of file names
#' @param ext string new extension, with or without the dot
#'
#' @return string or vector of strings of modified file names
#'
#' @export
change_extension <- function(path, ext) {
  add_extension(remove_extension(path), ext)
}


#' Overloads basename with option to remove extensions
#'
#' @param path string or vector of strings of file names
#' @param remove.extension logical whether to remove extension (including dot),
#'     defaults to FALSE
#'
#' @examples
#' basename("/dir1/dir2/file.ext", remove.extension = TRUE)
#' basename("/dir1/dir2/file.name.ext", remove.extension = TRUE)
#' basename("/dir1/dir2/file.ext")
#'
#' @export
basename <- function(path, remove.extension = FALSE) {
  if(remove.extension == TRUE) {
    remove_extension(base::basename(path))
  } else base::basename(path)
}


#' Overloads dirname with option to remove URL scheme ('http://', etc.)
#'     Also changes dirname behavior so that the last part of a URL ending in /
#'     counts as a directory, not as a file
#'
#' @param path string or vector of strings of file names
#' @param remove.scheme logical whether to remove URL scheme,
#'     defaults to FALSE
#'
#' @examples
#' dirname("/dir1/dir2/file.ext", remove.scheme = TRUE)
#' dirname("/dir1/dir2/file.ext")
#'
#' @export
dirname <- function(path, remove.scheme = FALSE) {
  path <- gsub("\\/$", "/file_to_be_remove.ext", path)
  if(remove.scheme) {
    gsub("[a-zA-Z0-9]+:\\/?\\/?", "", base::dirname(path))
  } else base::dirname(path)
}


#' Get URL scheme ('http://', etc.)
#'
#' @param path string or vector of strings of file names
#' @param colonslashes whether to include '://' and ':', defaults to TRUE
#'
#' @export
url_scheme <- function(path, colonslashes = TRUE) {
  if(colonslashes) {
    gsub("^([a-zA-Z0-9]+:\\/?\\/?).*", "\\1", path)
  } else gsub("^([a-zA-Z0-9]+):\\/?\\/?.*", "\\1", path)
}
