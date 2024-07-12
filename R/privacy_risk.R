#' Create a sequence with which to replace IDs
#'
#' This function creates an integer sequence matching distinct values
#' in a column of the input tbl. The resulting crosswalk can be used
#' to replace IDs in datasets when unique identifiers that are not
#' stable across datasets are needed.
#'
#' @param input A tbl containing the column of interest
#' @param id_col The name of the column containing IDs to be mapped.
#' @param rep_col The name of the column to contain the replacement
#'   integers.
#'
#' @return A two-column tbl containing unique values from id_col and
#'   replacement values in rep_col
#' @export
#' @md
gen_xwalk <- function(input, id_col, rep_col = 'seq_id')
  get_argos_default()$gen_xwalk(input, id_col, rep_col)

argos$set(
  'public', 'gen_xwalk',
  #' @name gen_xwalk-method
  #' @inherit gen_xwalk
  function(input, id_col, rep_col = 'seq_id') {
    idq <- quo_name(id_col)
    req <- quo_name(rep_col)

    input %>% select(!!idq) %>% distinct() %>%
      arrange_at(vars(!!idq)) %>% # Oracle insists on an ordering
      mutate( !!req := as.integer(row_number()))
  })

#' Add or replace one ID column with another
#'
#' This function takes a crosswalk between the values in a specific
#' column of a tbl and a new value, and either adds a column with the
#' new values or replaces the old values with the new.  It is intended
#' for use replacing an ID that should be redacted out of the data
#' with a lower-risk ID.
#'
#' @param data The tbl containing the data
#' @param id_col The name of the column containing the ID to join.
#'   Defaults to the name of data suffixed by `_id`
#' @param xwalk The tbl containing crosswalk between id_col and new
#'   values.  Defaults to using a sequence of integers.
#' @param replace  If TRUE, replace the contents of id_col with the
#'   first non-id_col column of xwalk.  If FALSE, leave the original
#'   id_col in place.
#'
#' @return The tbl with augmented or replaced IDs
#' @export
#' @md
new_id <- function(data,
                   id_col = paste0(deparse(substitute(data)), '_id'),
                   xwalk = select_at(data, vars(id_col)) %>% distinct() %>%
                     mutate(seq_id = row_number()),
                   replace = FALSE)
  get_argos_default()$new_id(data, id_col, xwalk, replace)


argos$set(
  'public', 'new_id',
  #' @name new_id-method
  #' @inherit new_id
  function(data,
           id_col = paste0(deparse(substitute(data)), '_id'),
           xwalk = select_at(data, vars(id_col)) %>% distinct() %>%
             mutate(seq_id = row_number()),
           replace = FALSE) {

    xform <- data %>% left_join(xwalk, by = id_col)
    if (replace) {
      newcol <- grep(id_col, tbl_vars(xwalk),
                     invert = TRUE, value = TRUE, fixed = TRUE)[1]
      xform <- xform %>% select(-one_of(id_col)) %>%
        rename_at(vars(newcol), function (x) { id_col })
    }
    xform
  })

#' Convert dates to ages
#'
#' For a simple tbl resembling the PEDSnet (or PCORnet, since both
#' use a `_date` suffix to denote dates) data model, create a new tbl in which
#' dates are replaced with ages in days.  Times (columns with names ending in
#' `_time` or `_datetime`) are removed.
#'
#' @param cohort The tbl to scrub
#' @param person_tbl The tbl containing person_id and birth_datetime
#'   to use for age calculations.
#'
#' @return The scrubbed tbl
#' @export
#' @md
dates_to_ages <- function(cohort, person_tbl = cdm_tbl('person'))
  get_argos_default()$dates_to_ages(cohort, person_tbl)

argos$set(
  'public', 'dates_to_ages',
  #' @name dates_to_ages-method
  #' @inherit dates_to_ages
  function(cohort, person_tbl = self$cdm_tbl('person')) {

    cohort_vars <- tbl_vars(cohort)

    if (! any(grepl('_date', cohort_vars))) return(cohort)

    if (any(cohort_vars == 'birth_date')) {
      cohort <- cohort %>% mutate(birth_dt = birth_date)
    }
    else {
      if (! any(cohort_vars == 'birth_datetime')) {
        cohort <- inner_join(cohort,
                             select(person_tbl, person_id, birth_datetime),
                             by = 'person_id')
      }
      cohort <- mutate(cohort,
                       birth_dt = sql('cast("birth_datetime" as date)'))
    }

    cohort <- cohort %>%
      mutate_at(vars(ends_with('_date')), list(age = ~(. - birth_dt))) %>%
      rename_at(vars(ends_with('_date_age')), list(~sub('_date', '', .))) %>%
      select_at(vars(-ends_with('_date'))) %>%
      select_at(vars(-ends_with('_datetime'))) %>%
      select_at(vars(-ends_with('_time'))) %>%
      select(-birth_dt)


    # Fix mutate_at()'s "helpful" removal of source column name when there is
    # only one column changed
    dates <- grep('_date', cohort_vars, value = TRUE)
    if (length(dates) == 1) {
      agenew <- sub('_date', '_age',dates[1])
      cohort <- cohort %>% rename(!! agenew := age)
    }
    cohort
  })


#' Replace person IDs and convert dates to ages
#'
#' For a simple table structure resembling the PEDSnet CDM, create
#' variants with nonce person IDs (via [new_id()]) and dates converted to ages
#' in days (via [dates_to_ages()]).
#'
#' @param cohort The tbl to scrub
#' @param person_xwalk The tbl containing person_id mappings for
#'   [new_id()].  If `NA` (the default), will create a nonce crosswalk
#'   based on the `person_id`s in `cohort`.  The crosswalk is not
#'   returned, so you should do this only if you don't ever need to
#'   walk the nonce IDs back to the originals.
#' @param person_tbl The tbl containing person_id and birth_datetime
#'   to use for age calculations.
#'
#' @return The scrubbed tbl
#' @export
#' @md
scrub_person_info <- function(cohort, person_xwalk = NA,
                              person_tbl = cdm_tbl('person'))
  get_argos_default()$scrub_person_info(cohort, person_xwalk, person_tbl)


argos$set(
  'public', 'scrub_person_info',
  #' @name scrub_person_info-method
  #' @inherit scrub_person_info
  function(cohort, person_xwalk = NA,
           person_tbl = self$cdm_tbl('person')) {
    if (any(is.na(person_xwalk))) {
      person_xwalk <- cohort %>% distinct(person_id) %>%
        self$gen_xwalk('person_id')
    }

    cohort %>%
      self$dates_to_ages(person_tbl) %>%
      self$new_id(id_col = 'person_id', xwalk = person_xwalk, replace = TRUE)
  })
