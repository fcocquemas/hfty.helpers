# Helpers for saving summary statistics during transformation of data.table


#' Count number of observations in a data.table summarized by a certain set of fields
#'
#' @param dt data.table to summarize
#' @param fields string or vector of strings, sets of fields to summarize the observations by.
#'    Use "-" between column names to summarize by multiple fields. Use a single "*" to return
#'    the unconditional number of rows of the data.table
#'
#' @export
# count_by <- function(dt, fields) {
#   # browser()
#   sapply(fields, function(f) {
#     f <- strsplit(f, "-")[[1]]
#     if(length(f) == 1 && f == "*") nrow(dt) else nrow(dt[, .N, by=c(f)])
#   })
# }
count_by <- function(df, fields) {
  nrow(df[, .N, by=c(fields)])
}
