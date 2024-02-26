#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import DBI
#' @importFrom dbplyr in_schema
#' @import dplyr
#' @import fs
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @import purrr
#' @importFrom readr read_csv write_csv
#' @import tibble
## usethis namespace: end
NULL

# Set up internal environment at package load
if (!exists('req_env')) req_env <- new.env(parent = emptyenv())

.init_run <- function() {
  # Invalidate cache when run starts
  config_rm('_codesets')

}
