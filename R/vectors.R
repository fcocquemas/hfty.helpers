# Helpers for working with vectors / sets

#' Pad list to a certain size with NAs or another value
#'
#' @param x vector or list
#' @param size target size after padding
#' @param pad what to pad with, defaults to NA_real_
#'
#' @examples
#' make_list_of_size(c(1, 2), 4)
#'
#' @export
make_list_of_size <- function(x, size, pad = NA_real_) {
  x <- as.list(x)
  if(length(x) < size) x <- c(x, rep(pad, size - length(x)))
  return(x)
}
