
# hfty.helpers

<!-- badges: start -->
<!-- badges: end -->

The goal of hfty.helpers is to provide, in package form, a few convenience functions I use in many different projects.

Install with `devtools::install_github("fcocquemas/hfty.helpers")`.

## Paths functions

- `file_path(...)`
- `extension(path, dot = TRUE)`
- `add_extension(path, ext)`
- `remove_extension(path)`
- `update_extension(path, ext)`
- `basename(path, remove.extensions = FALSE)`
- `dirname(path, remove.scheme = FALSE)`
- `url_scheme(path, colonslashes = TRUE)`


## Input/output

- `msg(object = NULL, quiet = FALSE, ...)`
- `read_csv_in_zip(zipfile, ...)`
- `save_rds(object, file, threads = parallel::detectCores(), compress_level = 9, quiet = FALSE)`

## Cloud: EC2

- `create_spot_instances(instance_params, conf = list(), spot_options = list())`
- `get_instances_ip(ec2_instances, conf = list())`
- `list_running_instances(conf = list())`

## Cloud: S3

- `s3_list_bucket(prefix, conf = list(), pattern = "", max = Inf, quiet = FALSE, ...)`
- `s3_object_exists(s3_url, conf = list(), quiet = FALSE, ...)`
- `s3_read(s3_url, conf = list(), args_read = list(), ...)`
- `s3_save(object, s3_url, conf = list(), args_save = list(), quiet = FALSE, ...)`


