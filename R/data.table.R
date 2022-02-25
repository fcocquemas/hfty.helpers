# Helpers for working with strings

library(data.table)

# Functions:
#   factor_column(dt, column_name)

#' Turn column to factor by reference
#'
#' Preserves attribute "label" of the column if it exists
#'
#' @param dt data.table source string to extract date from
#'
#' @examples
#' dt <- data.table(id=c("A", "A", "B"), value=c(1, 2, 3))
#' factor_column(dt, "id")
#'
#' @export
factor_column <- function(dt, column_name) {
  if(column_name %in% names(dt)) {
    current_label <- attr(dt[, get(column_name)], "label")
    dt[, c(column_name) := factor(get(column_name))]
    dt[, setattr(get(column_name), "label", current_label)]
  }
}
