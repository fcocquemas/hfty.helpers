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

#' Split vector in n parts of equal size (or close)
#'
#' If n is greater than the size of the vector, result will have
#' the size of the vector with one item per element.
#'
#' @param vec vector to split
#' @param n integer number of parts
#'
#' @return list of size n
#'
#' @examples
#' n_split(1:10, 2)
#' n_split(1:10, 3)
#'
#' @export
n_split <- function(vec, n) {
  split(vec, ceiling(seq_along(vec)/(length(vec)/n)))
}
