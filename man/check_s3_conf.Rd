% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3.R
\name{check_s3_conf}
\alias{check_s3_conf}
\title{Check configuration list, and get environment variables if missing}
\usage{
check_s3_conf(conf)
}
\arguments{
\item{conf}{list with 'key', 'secret', 'region', and 'base_url' parameters. If missing, will
look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION in environment variables.

Not exported}
}
\description{
Check configuration list, and get environment variables if missing
}
