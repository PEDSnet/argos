#' Create a new step log for results and progress info
#'
#' This function sets up the data structure used to track request progress in
#' the "step log".  As each step of request execution is completed, you should
#' add an event to the step log using [append_sum()] that summarizes the work
#' done.  The accumulated information can be used to describe the process of
#' data manipulation as well as the resource requirements for execution.
#'
#' The step log is a table of events saved during execution to summarize the
#' process. The content includes at least three columns for each event: `site`,
#' which is the name of the current site from `site_info.R`, `stamp`, which
#' contains the ISO-8601 current time, and `used`, which contains a summary of
#' CPU utilization to this point.
#'
#' Additional for a particular request is flexible, and is set by the key-value
#' pairs passed to init_sum().  This typically includes a descriptive name for
#' each step, and may include counts or other aggregate data resulting from that
#' step.  For cohort creation, these values might be used to create an attrition
#' table, while for modeling, these might include key parameter values from
#' successive rounds of fitting.  There is no constraint on the data recorded,
#' but values for the same names must be recorded at each step.
#'
#' @param ... Name-value pairs to be added to the step log
#' @param set_default A Boolean value which determines whether the
#'   result is stored as the default event accumulator.
#'
#' @return The step log, contaning the initial record
#' @seealso [append_sum()], [output_sum()]
#' @export
#' @md
init_sum <- function(..., set_default = TRUE)
  get_argos_default()$init_sum(..., set_default = TRUE)

argos$set(
  'public', 'init_sum',
  #' @name init_sum-method
  #' @inherit init_sum
  function(..., set_default = TRUE) {
    x <- tibble(!!! dots_list(...),
                qry_site = self$config('qry_site'),
                stamp = Sys.time(),
                used = paste(proc.time(), collapse = ';'))
    if (set_default) self$config('_step_log', x)
    x
  })

#' Add rows to a step log
#'
#' This function adds rows to an existing step log, filling in values
#' for the three extra columns created by [init_sum()] in addition to the data
#' passed in the current call.
#'
#' @param ... Additional name-value pairs to add.  These must match the names
#'   used to set up the step log in the call to [init_sum()].
#' @param step_log The step log in which to accumulate event information.
#'   If it is NA (the default), the default event accumulator is
#'   augmented.  Note that if you wish to override the default, you
#'   must pass this parameter by name.
#'
#' @return The step log with the new record added.
#' @seealso [init_sum()], [output_sum()]
#' @export
#' @md
append_sum <- function(..., step_log = NA)
  get_argos_default()$append_sum(..., step_log = step_log)

argos$set(
  'public', 'append_sum',
  #' @name append_sum-method
  #' @inherit append_sum
  function(..., step_log = NA) {
    events <- if (is.na(step_log)) self$config('_step_log') else step_log
    x <- dplyr::union(tibble(!!! dots_list(...),
                             qry_site = self$config('qry_site'),
                             stamp = Sys.time(),
                             used = paste(proc.time(), collapse = ';')),
                      events)
    if (is.na(step_log)) self$config('_step_log', x)
    x
  })

#' Output step log
#'
#' This function writes out the summary data accumulated by
#' [init_sum()] and [append_sum()].
#'
#' @param step_log The tbl in which event information was
#'   accumulated.  Defaults to the default event accumulator.
#' @param name The name to give the output file or table.
#' @param ... Additional arguments passed to [output_tbl()].
#'
#' @return The return value of [output_tbl()].
#' @seealso [init_sum()], [append_sum()]
#' @export
#' @md
output_sum <- function(step_log = get_argos_default()$config('_step_log'),
                       name = paste0(get_argos_default()$config('qry_site'), '_steps'),
                       ...)
  get_argos_default()$output_sum(step_log, name, ...)

argos$set(
  'public', 'output_sum',
  #' @name output_sum-method
  #' @inherit output_sum
  function(step_log = self$config('_step_log'),
           name = paste0(self$config('qry_site'), '_steps'),
           ...) {
    self$output_tbl(step_log, name = name, ...)
  })

#' Count unique elements in a resultset.
#'
#' Given a column (e.g. a foreign key), returns as an integer the
#' number of unique elements in that column. This can be used as one
#' of the outputs for [append_sum()] calls in the driver file.
#'
#' @param rs The [dplyr::tbl()] describing the resultset
#' @param id_col The name of the column in which to count elements.
#'
#' @return The number of distinct values
#' @export
#' @md
distinct_ct <- function(rs, id_col = 'person_id')
  get_argos_default()$distinct_ct(rs, id_col)

argos$set(
  'public', 'distinct_ct',
  #' @name distinct_ct-method
  #' @inherit distinct_ct
  function(rs, id_col = 'person_id') {
    # A little clunky, but n_distinct() doesn't support quoting of
    # identifiers to the DBMS
    rs %>% select(all_of(id_col)) %>% distinct() %>%
      summarize(dist_ct = n()) %>% pull(dist_ct)
  })
