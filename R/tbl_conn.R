#' Connect to a database table using naming conventions
#'
#' This function passes the table name information to [qual_name()] to construct
#' a fully-qualified table name, then opens a connection to that table.
#'
#' @inheritParams qual_name
#' @param db The database connection to use when accessing the table
qual_tbl <-
  function(name, schema_tag, db = config('db_src'))
    get_argos_default()$qual_tbl(name, schema_tag, db)

argos$set(
  'public', 'qual_tbl',
  #' @name qual_tbl-method
  #' @inherit qual_tbl
  function(name, schema_tag, db = self$config('db_src')) {
    tbl(db, self$qual_name(name, schema_tag, db))
})


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
    get_argos_default()$cdm_tbl(name, db)

argos$set(
  'public', 'cdm_tbl',
  #' @name cdm_tbl-method
  #' @inherit cdm_tbl
  function(name, db = self$config('db_src')) {
    self$qual_tbl(name, 'cdm_schema', db)
  })

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
    get_argos_default()$vocabulary_tbl(name, db)

argos$set(
  'public', 'vocabulary_tbl',
  #' @name vocabulary_tbl-method
  #' @inherit vocabulary_tbl
  function(name, db = self$config('db_src')) {
    self$qual_tbl(name, 'vocabulary_schema', db)
  })


#' Connect to a result table
#'
#' This function sets up the connection to a database table that was presumably
#' created by during execution of this or a prior data request, and is found
#' in the schema designated for results, whether the table itself is intended to
#' be permanent or just an intermediate for this run.
#'
#' @param name The name of the table
#' @param db The database connection; you will rarely need to specify this.
#' @param results_tag The request tag to add to the table name
#'    (see [intermed_name()]).
#' @param local_tag The local tag to add to the table name
#'    (see [intermed_name()]).
#'
#' @return A [dplyr::tbl()]] pointing to the table
#' @seealso [intermed_name()], for more information on how the table
#'   specification is determined.
#' @export
#' @md
results_tbl <-
  function(name, db = config('db_src'), results_tag = TRUE, local_tag = FALSE)
    get_argos_default()$results_tbl(name, db, results_tag, local_tag)

argos$set(
  'public', 'results_tbl',
  #' @name results_tbl-method
  #' @inherit results_tbl
  function(name, db = self$config('db_src'),
           results_tag =  TRUE, local_tag = FALSE) {
    self$qual_tbl(self$intermed_name(name, temporary = FALSE,
                                     results_tag = results_tag,
                                     local_tag = local_tag),
                  'results_schema', db)
  })
