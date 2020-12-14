# Helpers for saving summary statistics during transformation of data.table


#' Count number of observations in a data.table summarized by a certain set of fields
#'
#' @param dt data.table to summarize
#' @param fields string or vector of strings, sets of fields to summarize the observations by.
#'    Use "-" between column names to summarize by multiple fields. Use a single "*" to return
#'    the unconditional number of rows of the data.table
#'
#' @examples
#' iris_dt <- as.data.table(iris)
#' count_by(iris_dt, c("*", "Species", "Species-Petal.Width"))
#'
#' @import data.table
#' @export
count_by <- function(dt, fields) {
  # browser()
  sapply(fields, function(f) {
    f <- strsplit(f, "-")[[1]]
    if(length(f) == 1 && f == "*") nrow(dt) else nrow(dt[, .N, by=c(f)])
  })
}


#' Count observations dropped from data.table by running expr and save as attribute
#'
#' @param dt data.table to transform and summarize
#' @param fields string or vector of strings, sets of fields to summarize the observations by.
#'    Use "-" between column names to summarize by multiple fields. Use a single "*" to return
#'    the unconditional number of rows of the data.table
#' @param key_name string list key to save the statistics under, in the "log" attribute of the
#'    data.table
#' @param expr
#'
#' @return input data.table after transform from expr
#'
#' @examples
#' iris_dt <- as.data.table(iris)
#' iris_dt <- filter_count(iris_dt, c("*", "Species", "Species-Petal.Width"),
#'   "setosa-only", { iris_dt[Species == "setosa"] })
#' attr(iris_dt, "log")
#'
#' @export
filter_count <- function(dt, fields, key_name, expr) {
  # Save attribute log (if it exists) pre-operation
  count_attr <- as.list(attr(dt, "log"))

  # Count fields pre-operation
  counts_pre <- count_by(dt, fields)

  # Run operation
  expr <- rlang::enquo(expr)
  dt <- rlang::eval_tidy(expr)

  # Count fields post-operation
  counts_post <- count_by(dt, fields)

  # Add count to attributes and return transformed data.table
  count_attr[key_name] <- list(rbind(counts_pre, counts_post))
  setattr(dt, "log", count_attr)

  return(dt)
}
