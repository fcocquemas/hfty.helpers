% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{read_csv_in_zip}
\alias{read_csv_in_zip}
\title{Read CSV inside of zip file using data.table's fread
Works with any regular delimited file type recognized by fread.
Tries to use pigz, otherwise unzip if unavailable. Adds a line break at the end of the file}
\usage{
read_csv_in_zip(zipfile, ...)
}
\arguments{
\item{zipfile}{string path to file containing a single CSV file}

\item{...}{additional parameters are passed to fread}
}
\description{
Read CSV inside of zip file using data.table's fread
Works with any regular delimited file type recognized by fread.
Tries to use pigz, otherwise unzip if unavailable. Adds a line break at the end of the file
}
