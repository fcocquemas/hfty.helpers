% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3.R
\name{s3_object_exists}
\alias{s3_object_exists}
\title{Check if object exists in cloud}
\usage{
s3_object_exists(s3_url, conf = list(), quiet = FALSE, ...)
}
\arguments{
\item{s3_url}{string S3 URL to check. Needs to include the bucket and protocol (s3://bucket_name/)}

\item{conf}{list with 'key', 'secret', 'region', and 'base_url' parameters. If missing, will
look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION in environment variables.}

\item{quiet}{logical turn off output messages, default to FALSE}

\item{...}{additional arguments passed to}
}
\value{
TRUE or FALSE depending on whether the object is found
}
\description{
Check if object exists in cloud
}
\examples{
\dontrun{
s3_object_exists("s3://hfty-test-bucket/text_file.txt")
s3_object_exists("s3://hfty-test-bucket/text_file.txt", conf = list(region = "us-east-2"))
}

}
