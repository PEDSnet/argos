.qual_tbl <-
  function(name, schema_tag, db = config('db_src'))
    tbl(db, .qual_name(name, schema_tag, db))

#' Get DBI-compatible connection handle across dplyr generations
#'
#' Given a database connection the might have been created by [DBI::dbConnect()]
#'   or my one of the `src_foo()` functions in versions of dbplyr, return a
#'   DBI-compatible connection.
#'
#' Note that this function is essentially identical to [dbplyr::remote_con()]
#'   for dbplyr connections.
#'
#' @param db A DBI or dbplyr connection.
#'
#' @return A DBI-compatible connection, or NULL if `db` is not a connection.
#' @export
#' @md
dbi_con <- function(db) {
  # R introspection is very limited, so best approach is to keep trying.
  if (! any(is.object(db))) return(NULL)
  rslt <- NULL
  # DBI Connection
  if (! is.null(tryCatch( DBI::dbDataType(db, 1L),
                          error = function (e) NULL))) return(db)
  # Modern dbplyr
  if (any(class(db) == 'tbl_sql') &&
      ! is.null(tryCatch( { rslt <- remote_con(db) },
                          error = function (e) NULL))) return(rslt)
  # Older dbplyr query - breaks encapsulation
  if (exists('src', where = db) &&
      ! is.null(tryCatch( { rslt <- db$src$con },
                          error = function (e) NULL))) return(rslt)
  # Older dbplyr connection - breaks encapsulation
  if (exists('con', where = db) &&
      ! is.null(tryCatch( { rslt <- db$con },
                          error = function (e) NULL))) return(rslt)
  rslt
}

#' Connect to an existing CDM data table
#'
#' @param name The name of the table
#' @param db The database connection; you will rarely need to specify this.
#'
#' @return A [dplyr::tbl()]] pointing to the table
#' @export
#' @md
cdm_tbl <-
  function(name, db = config('db_src'))
    .qual_tbl(name, 'cdm_schema', db)

#' Connect to an existing CDM vocabulary table
#'
#' @param name The name of the table
#' @param db The database connection; you will rarely need to specify this.
#'
#' @return A [dplyr::tbl()]] pointing to the table
#' @export
#' @md
vocabulary_tbl <-
  function(name, db = config('db_src'))
    .qual_tbl(name, 'vocabulary_schema', db)


#' Connect to a result table
#'
#' This function sets up the connection to a database table that was presumably
#' created by during execution of this or a prior data request, and is found
#' in the schema designated for results, whether the table itself is intended to
#' be permanent or just an intermediate for this run.
#'
#' @param name The name of the table
#' @param db The database connection; you will rarely need to specify this.
#' @param results_tag The request tag to add to the table name (see [intermed_name()]).
#' @param local_tag The local tag to add to the table name (see [intermed_name()]).
#'
#' @return A [dplyr::tbl()]] pointing to the table
#' @seealso [intermed_name()], for more information on how the table
#'   specification is determined.
#' @export
#' @md
results_tbl <- function(name, db = config('db_src'),
                        results_tag =  TRUE, local_tag = FALSE) {
    .qual_tbl(intermed_name(name, temporary = FALSE,
                            results_tag = results_tag,
                            local_tag = local_tag),
              'results_schema', db)
}
