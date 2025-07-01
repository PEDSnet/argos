#' Like compute(), but insure the target doesn't exist and allow for trace output
#'
#' This function creates a database table containing the results of a dplyr
#' expression, whether or not a table by that name already exists.  If tracing
#' of database activity is turned on, it generates logging as well.
#'
#' One of the limitations of [dplyr::compute()] as applied to database tables is
#' that it does not support the `overwrite` parameter, so calls are not
#' idempotent: if the target table already exists, an error occurs.  This
#' function simply drops the target table if it exists.  Note that there is no
#' `overwrite` vs `append` option here; the target is always created anew.
#'
#' While the underlying mechanism is the same, [compute_new()] is generally
#' intended for creation of intermediate tables during computation, and
#' [output_tbl()] for generation of final output.  The latter is more aware of
#' different options for saving results, and will direct data appropriately.
#'
#' Similarly, whether a table is temporary or permanent is meant to be
#' controlled by the user's configuration settings.  It is possible to override
#' this by using the `temporary` parameter, but this should be done only in
#' cases where there is a pressing reason to insist on particular behavior
#' (e.g. computation of an intermediate linking table that is prohibitively
#' large).
#'
#' @param tblx The [dplyr::tbl()] expression to compute
#' @param name The name to give the table
#' @param temporary A Boolean indicator of whether the table should be
#'   temporary.
#' @param ... Other arguments passed to [dplyr::compute()]
#'
#' @return A [dplyr::tbl()] resulting from [dplyr::compute()].
#' @seealso [output_tbl()] for permanent output, [collect_new()] and
#'   [copy_to_new()] for database interactions with similar tracing
#' @export
#' @md
compute_new <- function(tblx,
                        name = paste0(sample(letters, 12, replace = TRUE),
                                      collapse = ""),
                        temporary = ! config('retain_intermediates'),
                        ...)
  get_argos_default()$compute_new(tblx, name, temporary, ...)

argos$set(
  'public', 'compute_new',
  #' @name compute_new-method
  #' @inherit compute_new
  function(tblx,
           name = paste0(sample(letters, 12, replace = TRUE),
                         collapse = ""),
           temporary = ! self$config('retain_intermediates'),
           ...) {
    if (!inherits(name, c('ident_q', 'dbplyr_schema')) && length(name) == 1) {
      name <- gsub('\\s+','_', name, perl = TRUE)
      name <- self$intermed_name(name, temporary)
    }
    con <- self$dbi_con(tblx)
    # Broken in dbplyr, so do it ourselves
    if (self$db_exists_table(con, name)) self$db_remove_table(con, name)
    if (self$config('db_trace')) {
      show_query(tblx)
      if (self$config('can_explain')) explain(tblx)
      message(' -> ',
              base::ifelse(packageVersion('dbplyr') < '2.0.0',
                           dbplyr::as.sql(name),
                           dbplyr::as.sql(name, con)))
      start <- Sys.time()
      message(start)
    }
    rslt <- dplyr::compute(tblx, name = name, temporary = temporary, ...)
    if (self$config('db_trace')) {
      end  <- Sys.time()
      message(end, ' ==> ', format(end - start))
    }
    rslt
  })


#' Like collect(), but allow for trace output
#'
#' This function is a simple wrapper for [dplyr::collect()], but allows for
#' logging when tracing of database activity is turned on.
#'
#' @param tblx The [dplyr::tbl()] expression to collect
#' @param ... Other arguments passed to [dplyr::collect()]
#'
#' @return A tbl resulting from [dplyr::collect()].
#' @seealso [compute_new()] and [copy_to_new()] for database interactions
#'   with similar tracing
#' @export
#' @md
collect_new <- function(tblx, ...) get_argos_default()$collect_new(tblx, ...)

argos$set(
  'public', 'collect_new',
  #' @name collect_new-method
  #' @inherit collect_new
  function(tblx, ...) {
    if (self$config('db_trace')) {
      if (inherits(tblx, 'tbl_sql')) {
        show_query(tblx)
        if (self$config('can_explain')) explain(tblx)
      }
      message(' -> collect')
      start <- Sys.time()
      message(start)
    }
    rslt <- collect(tblx)
    if (self$config('db_trace')) {
      end  <- Sys.time()
      message(end, ' ==> ', format(end - start))
    }
    rslt
  })

#' Like copy_to(), but allow for trace output
#'
#' This function is a simple wrapper around copy_to(), that provides
#' some trace output, allowing that there is less query construction
#' that occurs than in functions starting work in the database.  It
#' also works around a bug in dplyr that renders `overwrite`
#' non-functional in some circumstances.  Finally, it allows you to specify
#' that a large `df` be written out in smaller chunks, since not all DBI
#' database backends are capable of this directly.
#'
#' @param dest A remote data source.
#' @param df The data frame/tbl to be copied to the database
#' @param name The name to give the resulting table.  Defaults to the
#'   name of the data frame as qualified by intermed_name().
#' @param overwrite Whether to remove an existing table of the same
#'   name.  Defaults to TRUE, which is different from the dplyr
#'   function of the same name.
#' @param temporary Whether the created table should be temporary.
#'   Defaults to the opposite of `config('retain_intermediates')`.
#' @param ... Other arguments passed to dplyr::copy_to()
#' @param .chunk_size An integer specifying that the data should be written
#'   out in chunks of the specified number of rows.
#'
#' @return A tbl pointing to the remote table
#' @seealso [compute_new()] and [collect_new()] for database interactions
#'   with similar tracing
#' @export
#' @md
copy_to_new <- function(dest = config('db_src'), df,
                        name = deparse(substitute(df)),
                        overwrite = TRUE,
                        temporary = ! config('retain_intermediates'),
                        ..., .chunk_size = NA)
  get_argos_default()$copy_to_new(dest, df, name, overwrite,
                                  temporary, ..., .chunk_size)

argos$set(
  'public', 'copy_to_new',
  #' @name copy_to_new-method
  #' @inherit copy_to_new
  function(dest = self$config('db_src'), df,
           name = deparse(substitute(df)),
           overwrite = TRUE,
           temporary = ! self$config('retain_intermediates'),
           ..., .chunk_size = NA) {
    name <- self$intermed_name(name, temporary = temporary)
    if (self$config('db_trace')) {
      message(' -> copy_to')
      start <- Sys.time()
      message(start)
      message('Data: ', deparse(substitute(df)))
      message('Table name: ',
              base::ifelse(packageVersion('dbplyr') < '2.0.0',
                           dbplyr::as.sql(name),
                           dbplyr::as.sql(name, dbi_con(dest))),
              ' (temp: ', temporary, ')')
      message('Data elements: ', paste(tbl_vars(df), collapse = ','))
      message('Rows: ', NROW(df))
    }
    if (overwrite &&
        self$db_exists_table(dest, name)) {
      self$db_remove_table(dest, name)
    }
    dfsize <- tally(ungroup(df)) %>% pull(n)
    if (is.na(.chunk_size)) .chunk_size <- dfsize
    cstart <- 1
    if (.chunk_size < dfsize)
      cli::cli_progress_bar('Copying data: ', total = 100, clear = FALSE,
                            format = 'Copying data {cli::pb_bar} {cli::pb_percent}')
    while (cstart < dfsize) {
      cend <- min(cstart + .chunk_size, dfsize)
      rslt <- dplyr::copy_to(dest = dest,
                             df = slice(ungroup(df), cstart:cend), name = name,
                             append = TRUE, temporary = temporary, ...)
      if (.chunk_size < dfsize) cli::cli_progress_update(set = 100L * cend / dfsize)
      cstart <- cend + 1L
    }
    if (self$config('db_trace')) {
      end  <- Sys.time()
      message(end, ' ==> ', format(end - start))
    }
    rslt
  })


# Write data to a CSV file in a the appropriate results dir
.output_csv <- function(self, data, name = NA, local = FALSE, append = FALSE) {
  if (is.na(name)) name <- quo_name(enquo(data))
  dirs <- self$config('subdirs')
  self$collect_new(data) %>%
    write_csv(file.path(self$config('base_dir'),
                        base::ifelse(local, dirs$local_dir, dirs$result_dir),
                        paste0(name, '.csv')), na = '', append = append)
}

#' Output contents of a tbl
#'
#' Write the contents of a tbl to either a CSV file in the appropriate
#' results directory, or to the database, for future reference.
#'
#' This function is the switch point for output from a data request that is
#' meant to be permanent, such as final results or a cohort list that will be
#' reused in future data requests. It classifies output in one of two ways,
#' depending on the value of `local`.  If true, the output is intended to be
#' reviewed or retained by whoever executed the request, but not transmitted
#' back to the requestor.  If false, the output is intended for the requestor.
#' How this translates into practice depends on the destination.  If the output
#' is directed to a file, then the directory is chosen based on this flag, but
#' the file's name is constant.  If the output is directed to a database table,
#' then the local flag has the effect described in [intermed_name()].
#'
#' The decision about where to direct the output should generally be made based
#' on the execution mode of the request.  During development, the database is
#' the preferred target, since it is convenient for further analysis and
#' debugging.  When the request is distributed to a remote site for execution,
#' however, the workflow is typically to return the results to the requestor, so
#' they are typically directed to a file.  For local production runs, results
#' are saved in both locations: the databasw for collaboration and archiving
#' (possibly after merging with results from remote sites), and local files for
#' inspection and report writing.  As with other decisions about output, you
#' should generally not override these configuration-determined results unless
#' there is a pressing reason (e.g. you need to save output to a database table
#' because you know it will be used in a future request).
#'
#' @param data The tbl to write.
#' @param name The name (only) of the output  Defaults to the name of `data`.
#' @param local A value indicating whether the data should be written to a
#'   local-only location, which is not typically returned with query results.
#' @param file An indicator of whether the data should be written out as a CSV
#'   file.  If it is FALSE, no file will be written. If it is TRUE, a CSV file
#'   will be written named _name_, with a `.csv` suffix appended.
#' @param db A database connection, to which to write the results table.  If it
#'   is FALSE or NA, no database table is written.  If it is TRUE, then
#'   config('db_src') is used.
#' @param results_tag A value indicating whether to add a request tag to the
#'   output name (see [intermed_name()]) iff the results are written to the
#'   database.
#' @param append If TRUE, append content to any existing table or file,
#'   otherwise overwrite any existing content.  Note that it is not possible to
#'   append a database table or query to an existing database table.
#' @param ... Additional arguments passed to the database table creation
#'   function.
#'
#' @return The result of the operation, typically a copy of data,
#'   possibly collect()ed.
#' @seealso [compute_new()] and [copy_to_new()] for creating
#'   intermediate tables for internal use during request execution
#' @export
#' @md
output_tbl <- function(data, name = NA, local = FALSE,
                       file = base::ifelse(config('results_target') == 'file',
                                           TRUE, FALSE),
                       db = if (! file) config('results_target') else NA,
                       results_tag = TRUE, append = FALSE, ...)
  get_argos_default()$output_tbl(data, name, local, file, db,
                                 results_tag, append, ...)

argos$set(
  'public', 'output_tbl',
  #' @name output_tbl-method
  #' @inherit output_tbl
  function(data, name = NA, local = FALSE,
           file = base::ifelse(self$config('results_target') == 'file',
                               TRUE, FALSE),
           db = if (! file) self$config('results_target') else NA,
           results_tag = TRUE, append = FALSE, ...) {
    if (is.na(name)) name <- quo_name(enquo(data))

    if (file) {
      rslt <- .output_csv(self, data, name, local, append)
    }

    # Conditional logic is a little convoluted here to allow for legacy behavior
    # of allowing Boolean value for db
    if ( is.object(db) || (!is.na(db) && db)) {
      if (is.logical(db)) db <- self$config('db_src')
      rname <- self$intermed_name(name, temporary = FALSE, db = db,
                                  results_tag = results_tag, local_tag = local)
      if (any(class(data)  == 'tbl_sql') &&
          identical(self$dbi_con(data), self$dbi_con(db))) {
        if (append)
          cli::cli_abort(paste0('The {.var append} parameter cannot be TRUE ',
                                'if {.var data} is an database table or query'))
        rslt <- self$compute_new(data, rname, temporary = FALSE, ...)
      }
      else {
        rslt <- self$copy_to_new(db, self$collect_new(data), rname,
                                 temporary = FALSE,
                                 overwrite = !append, ...)
      }
    }

    invisible(rslt)
  })
