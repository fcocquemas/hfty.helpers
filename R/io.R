# Helpers for input/output

# Functions:
#   msg(object = NULL, quiet = FALSE, ...)
#   read_csv_in_zip(zipfile, ...)
#   save_rds(object, file, threads = parallel::detectCores(), compress_level = 9, quiet = FALSE)

#' Extends message with quiet option
#' You may use multiple objects like in message(), but they need to be passed after quiet = FALSE
#'
#' @param object object passed to message(), default to NULL
#' @param quiet logical turn off output messages, default to FALSE
#' @param ... additional objects or parameters passed to message()
#'
#' @examples
#' msg("Usage note")
#' msg(quiet = FALSE, "Usage note: ", "please wash your hands")
#'
#' @export
msg <- function(object = NULL, quiet = FALSE, ...) {
  if(quiet == FALSE) message(object, ...)
}

#' Read CSV inside of zip file using data.table's fread
#' Works with any regular delimited file type recognized by fread.
#' Tries to use pigz, otherwise unzip if unavailable. Adds a line break at the end of the file
#'
#' @param zipfile string path to file containing a single CSV file
#' @param ... additional parameters are passed to fread
#'
#' @export
read_csv_in_zip <- function(zipfile, ...) {
  if(is.null(attr(suppressWarnings(system("which pigz", intern = TRUE)), "status"))) {
    # Use pigz
    data.table::fread(cmd=paste0('pigz -dc ', zipfile, '; echo "" '), ...)
  } else {
    # Default to unzip
    data.table::fread(cmd=paste0('unzip -cq ', zipfile, '; echo "" '), ...)
  }
}

#' Save to rds using multiple threads (requires xz or pixz)
#'
#' @param x object to save
#' @param file string file path to save to
#' @param threads integer number of threads to use, default to parallel::detectCores()
#' @param compress_level integer xz compression level from 0 to 9, default to 9 (max)
#' @param quiet logical turn off output messages, default to FALSE
#'
#' @export
save_rds  <- function(x, file,
                      threads = parallel::detectCores(), compress_level = 9L,
                      quiet = FALSE) {
  if(is.na(threads)) threads <- 1

  if(!compress_level %in% 0L:9L) {
    stop("compress level must be an integer between 0 and 9")
  }

  if(is.null(attr(suppressWarnings(system("which xz", intern = TRUE)), "status"))) {
    # Use xz
    con <- pipe(paste0("xz -T", threads, " -", compress_level, " -f > ", file), "wb")
  } else if(is.null(attr(suppressWarnings(system("which pixz", intern = TRUE)), "status"))) {
    # Use pixz
    con <- pipe(paste0("pixz -p ", threads, " -", compress_level, " > ", file), "wb")
  } else {
    stop("Must have xz or pixz on PATH")
  }

  msg("Using ", threads, " threads for compression.", quiet = quiet)
  on.exit(if(exists("con")) close(con))
  saveRDS(x, file = con)
}
