% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumstats.R
\name{filter_count}
\alias{filter_count}
\title{Count observations dropped from data.table by running expr and save as attribute}
\usage{
filter_count(dt, fields, key_name, expr)
}
\arguments{
\item{dt}{data.table to transform and summarize}

\item{fields}{string or vector of strings, sets of fields to summarize the observations by.
Use "-" between column names to summarize by multiple fields. Use a single "*" to return
the unconditional number of rows of the data.table}

\item{key_name}{string list key to save the statistics under, in the "log" attribute of the
data.table}

\item{expr}{}
}
\value{
input data.table after transform from expr
}
\description{
Count observations dropped from data.table by running expr and save as attribute
}
\examples{
iris_dt <- as.data.table(iris)
iris_dt <- filter_count(iris_dt, c("*", "Species", "Species-Petal.Width"),
  "setosa-only", { iris_dt[Species == "setosa"] })
attr(iris_dt, "log")

}
