# Helpers for date and time

#' Find third Friday of month of a given date
#'
#' @param date Date or character representation of date
#'
#' @return Date object of the third Friday of month
#'
#' @examples
#' third_friday_of_month("2020-12-14")
#'
#' @export
third_friday_of_month <- function(date) {
  RcppBDT::getNthDayOfWeek(3, 5, RcppBDT::getMonth(as.Date(date)), RcppBDT::getYear(as.Date(date)))
}
