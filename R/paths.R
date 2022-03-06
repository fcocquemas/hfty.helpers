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
#   list_files(path = ".", pattern = NULL, all.files = TRUE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE)

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
#' @param url string or vector of strings of URLs
#' @param colonslashes whether to include '://' and ':', defaults to TRUE
#'     Not included when scheme is missing
#'
#' @examples
#' get_url_scheme("http://example.com")
#' get_url_scheme("https://www.example.org/index.html")
#' get_url_scheme("s3://hfty-test-bucket/dir1/file.txt", colonslashes = FALSE)
#' @export
get_url_scheme <- function(url, colonslashes = TRUE) {
  scheme <- httr::parse_url(url)$scheme
  if(is.null(scheme)) {
    scheme <- ""
  } else if(colonslashes & scheme != "") {
    colonslashes <- gsub(paste0("^", scheme, "(:\\/?\\/?).*"), "\\1", url)
    scheme <- paste0(scheme, colonslashes)
  }

  return(scheme)
}

#' Get hostname from URL
#'
#' @param url string or vector of strings of URLs
#'
#' @examples
#' get_path("http://example.com")
#' get_path("https://www.example.org/index.html")
#' get_path("s3://hfty-test-bucket/dir1/file.txt")
#'
#' @export
get_hostname <- function(url) {
  httr::parse_url(url)$hostname
}

#' Get path from URL
#'
#' @param url string or vector of strings of URLs
#'
#' @examples
#' get_path("http://example.com")
#' get_path("https://www.example.org/index.html")
#' get_path("s3://hfty-test-bucket/dir1/file.txt")
#'
#' @export
get_path <- function(url) {
  httr::parse_url(url)$path
}

#' Wraps list.files with better defaults (all files, full paths,
#' recursive, no dots)
#'
#' @param path vector of full path names; defaults to working directory
#' @param pattern optional regular expression to filter paths
#' @param all.files logical, default TRUE
#' @param full.names logical, default TRUE
#' @param recursive logical, default TRUE
#' @param ignore.case logical, default FALSE
#' @param include.dirs logical, default FALSE
#' @param no.. logical, default TRUE
#'
#' @examples
#' list_files(".")
#'
#' @export
list_files <- function(
  path = ".", pattern = NULL, all.files = TRUE, full.names = TRUE,
  recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE) {
  list.files(path, pattern, all.files, full.names, recursive, ignore.case,
             include.dirs, no..)
}
