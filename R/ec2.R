# Helpers for working with Amazon EC2

# Functions:
#   create_spot_instances(instance_params, conf = list(), spot_options = list())
#   get_instances_ip(ec2_instances, conf = list())
#   list_running_instances(conf = list())

#' Check configuration list, and get environment variables if missing
#'
#' @param conf list with 'key', 'secret', 'region', and 'base_url' parameters. If missing, will
#'     look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION in environment variables.
#'
#' Not exported
check_ec2_conf <- function(conf) {
  conf$key <- ifelse(is.null(conf$key), Sys.getenv("AWS_ACCESS_KEY_ID"), conf$key)
  conf$secret <- ifelse(is.null(conf$secret), Sys.getenv("AWS_SECRET_ACCESS_KEY"), conf$secret)
  conf$region <- ifelse(is.null(conf$region), Sys.getenv("AWS_DEFAULT_REGION"), conf$region)
  conf$region <- ifelse(is.null(conf$region), "us-east-2", conf$region)
  return(conf)
}

#' Shortcut function for launching EC2 spot instances
#'
#' @param instance_params list with instance_type, ami, n_instances, pem_file, security_group, subnet
#' @param conf list with 'key', 'secret', and 'region' parameters. If missing, will
#'    look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION
#'    in environment variables
#' @param spot_options list with optional spot instance options
#'
#' @export
create_spot_instances <- function(instance_params, conf = list(),
                                  spot_options = list()) {
  conf <- check_ec2_conf(conf)
  ec2_instances <- aws.ec2::run_instances(
    type = instance_params$instance_type,
    image = instance_params$ami,
    max = instance_params$n_instances,
    sgroup = instance_params$security_group,
    subnet = instance_params$subnet,
    keypair = list(keyMaterial = readr::read_file(instance_params$pem_file)),
    spot_options = spot_options,
    key = conf$key,
    secret = conf$secret,
    region = conf$region
  )

  return(ec2_instances)
}

#' Collect IP addresses from EC2 instances
#'
#' @param ec2_instances list of EC2 instances returned by aws.ec2
#' @param conf list with 'key', 'secret', and 'region' parameters. If missing, will
#'    look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION
#'    in environment variables
#'
#' @export
get_instances_ip <- function(ec2_instances, conf = list()) {
  conf <- check_ec2_conf(conf)
  sapply(ec2_instances, function(x)
    aws.ec2::get_instance_public_ip(x,
                                    key = conf$key,
                                    secret = conf$secret,
                                    region = conf$region))
}

#' List running instances
#'
#' @param conf list with 'key', 'secret', and 'region' parameters. If missing, will
#'    look for AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION
#'    in environment variables
#'
#' @export
list_running_instances <- function(conf = list()) {
  conf <- check_ec2_conf(conf)
  ec2_instances <- aws.ec2::describe_instances(
    key = conf$key,
    secret = conf$secret,
    region = conf$region
  )

  # Collect instance objects
  ec2_instances <- unlist(lapply(ec2_instances, function(x) x[["instancesSet"]]),
                recursive = FALSE)

  # Select running instances only
  ec2_instances <- lapply(ec2_instances,
                          function(x) if(x$instanceState$name == "running") x)
  ec2_instances <- ec2_instances[!sapply(ec2_instances, is.null)]

  return(ec2_instances)
}
