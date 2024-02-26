#' Add a site column to a tbl
#'
#' For multisite databases, a join is done to a linking table to add a
#' `site` column to the input.  For single-site databases, the value
#' of `config('qry_site')` is used as the content for the newly-added
#' `site` column.
#'
#' @param f_tbl the tbl that needs to be joined to obtain site
#'   information.  Must contain id_col.
#' @param site_tbl the tbl that contains the id_col and `site`
#'   information.
#' @param id_col The name of the column on which to join.
#'
#' @return The contents of `f_tbl` with a field added
#'   for site.
#' @export
#' @md
add_site <- function(f_tbl,
                     site_tbl = cdm_tbl('person'),
                     id_col = 'person_id') {
  if (any(tbl_vars(f_tbl) == 'site')) return(f_tbl)

  idq <- enquo(id_col)

  if (any(tbl_vars(site_tbl) == 'site')) {
    f_tbl <- f_tbl %>%
      left_join(select(site_tbl, !!idq, site), by = id_col,
                copy = !same_src(site_tbl, f_tbl))
  }
  else {
    val <- config('qry_site')
    if ( is.null(val) ) val <- 'unknown'
    f_tbl <- f_tbl %>% mutate('site' = val)
  }
  f_tbl
}

#' Find facts occurring within a specified time before an index date
#'
#' This function filters a tbl for events starting within a certain
#' number of days prior to an index date, supplied in a second tbl.
#'
#' @param data The data table.
#' @param index_tbl A tbl containing (at least) `person_id` and an
#'   index date column against which data will be filtered.
#' @param event_date_col The name of the column containing the event
#'   date of interest.  Defaults to the name of the data tbl, less any
#'   suffix `_occurrence`, with `_start_date` added.
#' @param index_date_col The name of the column in index_tbl
#'   containing the index date.  Defaults to `cohort_start_date` for
#'   compatibility with Atlas output.
#' @param link_col The name of the column on which to join data and index_tbl.
#' @param lookback The lookback interval in days. Defaults to 365.
#'
#' @return A tbl containing filtered rows of interest (i.e. that match
#'   on `person_id` and fall within the lookback interval)
#' @export
#' @md
lookback_facts <- function(data,
                           index_tbl,
                           event_date_col = NA,
                           index_date_col = 'cohort_start_date',
                           link_col = 'person_id',
                           lookback = 365L) {

  idq <- enquo(index_date_col)

  if (is.na(event_date_col)) {
    dname <- NA
    if (any(class(data) == 'tbl_dbi')) {
      dname  <- dbplyr:::tbl_desc(data)
      dname <- unlist(regmatches(dname,
                                 regexec('(\\w+)"?>', dname, perl = TRUE)))[2]
    }
    if (is.na(dname)) dname <- deparse(substitute(data))
    event_date_col <-
      paste0(sub('_occurrence$', '', dname, ignore.case = TRUE),
             '_start_date')
  }
  data %>%
    inner_join(select(index_tbl, person_id, !!idq),
               by = link_col,
               copy = ! same_src(data, index_tbl)) %>%
    filter(between(dbplyr::ident(index_date_col) -
                     dbplyr::ident(event_date_col),
                   0L, lookback)) %>%
    select(-!!idq)
}
