# Helpers for working with strings

library(stringr)

# Functions:
#   extract_ymd(x)

#' Find and extract date from string with regular expression
#'
#' @param x string source string to extract date from
#'
#' @examples
#' extract_ymd("some_string_2020-04-03-nokey")
#' extract_ymd("another20100505-02-04")
#' extract_ymd("nodatehere")
#'
#' @export
#'
extract_ymd <- function(x) {
  # Extract ymd strings
  x <- str_extract_all(x, "([0-9]{4}\\-?[0-9]{2}\\-?[0-9]{2})")

  # Get first date if multiple ones
  x <- sapply(x, function(y) y[1])

  # Convert to date with lubridate
  x <- lubridate::ymd(x)

  x
}
