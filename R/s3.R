# Helpers for working with Amazon S3 and S3-compatible stores

library(data.table)

# Functions:
#   s3_list_bucket(prefix, conf = list(), pattern = "", max = Inf, quiet = FALSE, ...)
#   s3_object_exists(s3_url, conf = list(), quiet = FALSE, ...)
#   s3_read(s3_url, conf = list(), args_read = list(), quiet = FALSE, ...)
#   s3_save(object, s3_url, conf = list(), args_save = list(), quiet = FALSE, ...)

#' Check configuration list, and get environment variables if missing
#'
#' @param conf list with 'key', 'secret', 'region', and 'base_url' parameters. If missing, will
#'     look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION in environment variables.
#'
#' Not exported
check_s3_conf <- function(conf) {
  conf$key <- ifelse(is.null(conf$key), Sys.getenv("AWS_ACCESS_KEY_ID"), conf$key)
  conf$secret <- ifelse(is.null(conf$secret), Sys.getenv("AWS_SECRET_ACCESS_KEY"), conf$secret)
  conf$region <- ifelse(is.null(conf$region), Sys.getenv("AWS_DEFAULT_REGION"), conf$region)
  conf$base_url <- ifelse(is.null(conf$base_url), "s3.amazonaws.com", conf$base_url)
  return(conf)
}

#' List S3 bucket content into data.table
#'
#' @param prefix string prefix of S3 files to select. Needs to include the URL scheme and
#'     bucket name (s3://bucket_name/)
#' @param conf list with 'key', 'secret', 'region', and 'base_url' parameters. If missing, will
#'     look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION in environment variables.
#' @param pattern string regexp to filter results
#' @param max integer max number of records to return, pre-filtering. Default to Inf
#' @param quiet logical turn off output messages, default to FALSE
#' @param ... additional arguments passed to aws.s3::get_bucket
#'
#' @examples
#' \dontrun{
#' s3_list_bucket("s3://hfty-test-bucket/")
#' s3_list_bucket("s3://hfty-test-bucket/", pattern = "\\.txt$")
#' }
#'
#' @export
s3_list_bucket <- function(prefix, conf = list(), pattern = "", max = Inf, quiet = FALSE, ...) {
  conf <- check_s3_conf(conf)

  if(url_scheme(prefix) == "") stop("prefix needs to include URL scheme (s3://) and bucket name")

  # List bucket using aws.s3
  bucket <- aws.s3::get_bucket(
    prefix = aws.s3::get_objectkey(prefix),
    bucket = aws.s3::get_bucketname(prefix),
    max = max,
    base_url = conf$base_url,
    key = conf$key,
    secret = conf$secret,
    region = conf$region)

  # Collect results
  if(length(bucket) > 0) {
    dt <- data.table::rbindlist(lapply(bucket, function(x) {
      # Used for Wasabi: Owner is multi-dimensional
      if(is.list(x[["Owner"]])) x[["Owner"]] <- x[["Owner"]][["DisplayName"]]
      data.table::as.data.table(x[names(x)]) }))
    dt$LastModified <- lubridate::ymd_hms(dt$LastModified)
    dt$Size <- as.numeric(dt$Size)

    # Filter by regex pattern
    if(pattern != "") {
      return(dt[grepl(pattern, dt$Key), ])
    } else {
      return(dt)
    }
  } else {
    msg("No files found.", quiet = quiet)
    NULL
  }
}

#' Check if object exists in cloud
#'
#' @param s3_url string S3 URL to check. Needs to include the bucket and protocol (s3://bucket_name/)
#' @param conf list with 'key', 'secret', 'region', and 'base_url' parameters. If missing, will
#'     look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION in environment variables.
#' @param quiet logical turn off output messages, default to FALSE
#' @param ... additional arguments passed to
#'
#' @return TRUE or FALSE depending on whether the object is found
#'
#' @examples
#' \dontrun{
#' s3_object_exists("s3://hfty-test-bucket/text_file.txt")
#' s3_object_exists("s3://hfty-test-bucket/text_file.txt", conf = list(region = "us-east-2"))
#' }
#'
#' @export
s3_object_exists <- function(s3_url, conf = list(), quiet = FALSE, ...) {
  conf <- check_s3_conf(conf)
  output <- utils::capture.output({
     resp <- aws.s3::object_exists(s3_url,
                        base_url = conf$base_url,
                        key = conf$key,
                        secret = conf$secret,
                        region = conf$region)
  }, type = "message")
  attr(resp, "message") <- output
  msg(output, quiet = quiet)
  return(resp)
}

#' Read S3 file to R guessing type from extension
#'
#' @param s3_url string S3 URL to read. Needs to include the bucket and protocol (s3://bucket_name/)
#' @param conf list with 'key', 'secret', 'region', and 'base_url' parameters. If missing, will
#'     look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION in environment variables.
#' @param args_read optional, additional arguments passed to read function
#' @param ... additional arguments passed to aws.s3::save_object
#'
#' @examples
#' \dontrun{
#' s3_read("s3://hfty-test-bucket/cars.rds")
#' s3_read("s3://hfty-test-bucket/cars.csv")
#' }
#'
#' @export
s3_read <- function(s3_url, conf = list(), args_read = list(), ...) {
  conf <- check_s3_conf(conf)

  # Download URL to temp file
  ext <- extension(s3_url)
  tmp <- tempfile(fileext = ext)
  on.exit(unlink(tmp))
  r <- aws.s3::save_object(
    object = aws.s3::get_objectkey(s3_url),
    bucket = aws.s3::get_bucketname(s3_url),
    file = tmp,
    base_url = conf$base_url,
    key = conf$key,
    secret = conf$secret,
    region = conf$region, ...)

  # Read temp file into R
  args <- as.list(c(file = tmp, if(length(args_read) > 0) args_read else NULL))
  if(ext == ".rds") {
    return(do.call(readRDS, args))
  } else if(ext == ".csv") {
    return(do.call(data.table::fread, args))
  # } else if(ext == ".fwf") {
    # return(do.call(rio::import, args))
  } else {
    stop("unsupported file type in s3_url")
  }

}

#' Save R object to S3 guessing type from extension
#'
#' @param object current session object to save
#' @param s3_url string S3 URL to save to. Needs to include the bucket and protocol (s3://bucket_name/)
#' @param conf list with 'key', 'secret', 'region', and 'base_url' parameters. If missing, will
#'     look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION in environment variables.
#' @param args_save  optional, additional arguments passed to save function
#' @param quiet logical turn off output messages, default to FALSE
#' @param ... additional arguments passed to aws.s3::put_object
#'
#' @examples
#' \dontrun{
#' s3_save(cars, "s3://hfty-test-bucket/cars.rds")
#' s3_save(cars, "s3://hfty-test-bucket/cars.csv")
#' s3_save(cars, "s3://hfty-test-bucket/cars.fwf")
#' }
#'
#' @export
s3_save <- function(object, s3_url, conf = list(), args_save = list(), quiet = FALSE, ...) {
  conf <- check_s3_conf(conf)

  # Save to temp file
  ext <- extension(s3_url)
  tmp <- tempfile(fileext = ext)
  on.exit(unlink(tmp))
  args <- as.list(c(x = list(object), file = tmp, if(length(args_save) > 0) args_save else NULL))
  if(ext == ".rds") {
    do.call(save_rds, args)
  } else if(ext == ".csv") {
    do.call(data.table::fwrite, args)
  } else if(ext == ".fwf") {
    do.call(rio::export, args)
  } else stop("unsupported file type in s3_url")

  # Upload to S3
  r <- aws.s3::put_object(
    file = tmp,
    object = aws.s3::get_objectkey(s3_url),
    bucket = aws.s3::get_bucketname(s3_url),
    base_url = conf$base_url,
    key = conf$key,
    secret = conf$secret,
    region = conf$region,
    ...)
  return(r)

}
