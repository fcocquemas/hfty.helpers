% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paths.R
\name{file_path}
\alias{file_path}
\title{Extends the file.path function to avoid doubled trailing slashes
unless preceded by colon (for protocol such as http://)}
\usage{
file_path(...)
}
\arguments{
\item{...}{character vectors passed to file.path after cleaning}
}
\description{
Extends the file.path function to avoid doubled trailing slashes
unless preceded by colon (for protocol such as http://)
}
\examples{
file_path("dir1/dir2//", "dir3", "file.ext")

}
