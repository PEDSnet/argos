# Internal function to check possible default for base_dir
.intuit_base_dir <- function() {
  preferred <- Sys.getenv('ARGOS_DATA_REQUEST_ROOT',
                          unset = Sys.getenv('HOME', unset = NA))
  if (is.na(preferred) || nchar(preferred) == 0 || ! file.exists(preferred)) {
    if (any(file.exists(c('run.R',
                          file.path('..', 'site', 'run.R'))))) {
      preferred <- file.path(getwd(), '..')
    } else if (file.exists(file.path('site', 'run.R'))) {
        preferred <- getwd()
    } else {
      preferred <- this.path::this.dir(n = 2, default = getwd())
    }
  }
  preferred
}

# Internal function to set up default subdirs list
.intuit_subdirs <- function(base) {
  subdirs <- list()
  for (d in c('code', 'specs', 'local', 'results', 'reporting', 'locode')) {
    if (file.exists(file.path(base, d))) subdirs[[d]] = d
  }
  subdirs
}

# Internal function to set up default table name mappings
.intuit_table_names <- function(cdm_type = 'pedsnet', case = 'lower') {
  cdm_type <- tolower(cdm_type)
  tn_map <- list()
  .fold <- if (case == 'upper') \(x) toupper(x) else \(x) x

  if (tolower(cdm_type) == 'pedsnet') {
    for (tn in
         c('adt_occurrence', 'care_site', 'cohort', 'cohort_definition',
           'condition_era', 'condition_occurrence', 'death', 'device_exposure',
           'dose_era', 'drug_era', 'drug_exposure', 'fact_relationship',
           'hash_token', 'immunization', 'location', 'location_fips',
           'location_history', 'measurement', 'measurement_anthro',
           'measurement_labs', 'measurement_vitals', 'measurement_organism',
           'note', 'note_nlp', 'observation', 'observation_period', 'person',
           'procedure_occurrence', 'provider', 'specialty', 'visit_occurrence',
           'visit_detail', 'visit_payer', 'concept', 'concept_ancestor',
           'concept_relationship')) tn_map[[tn]] = .fold(tn)
  } else if (cdm_type == 'omop') {
    for (tn in
         c('care_site', 'cdm_source', 'cohort', 'cohort_definition', 'cost',
           'condition_era', 'condition_occurrence', 'death', 'device_exposure',
           'dose_era', 'drug_era', 'drug_exposure', 'episode', 'episode_event',
           'fact_relationship', 'location',  'measurement', 'metadata', 'note',
           'note_nlp', 'observation', 'observation_period', 'payer_plan_period',
           'person', 'procedure_occurrence', 'provider',  'specimen',
           'visit_occurrence', 'visit_detail', 'visit_payer', 'concept',
           'concept_ancestor', 'concept_relationship')) tn_map[[tn]] = .fold(tn)
  } else if (cdm_type == 'pcornet') {
    for (tn in
         c('condition', 'death', 'death_cause', 'demographic', 'diagnosis',
           'dispensing', 'encounter', 'enrollment', 'external_meds', 'harvest',
           'hash_token', 'immunization', 'lab_history', 'lab_result_cm',
           'lds_address_history', 'med_admin', 'obs_clin', 'obs_gen',
           'pat_relationship', 'pcornet_trial', 'prescribing', 'pro_cm',
           'procedures', 'provider', 'vital')) tn_map[[tn]] = .fold(tn)
  }
  tn_map
}

#' Set up a working argos session
#'
#' When an `argos` session is created, it's largely an empty shell waiting for
#' additional configuration to enable interaction with a database. This method
#' allows you to set the most often used configuration items in one go,
#' providing sometimes-useful defaults for most of them.
#'
#' You're free to skip this and set the necessary configuration item by item,
#' as is done in the file-based arrangement often used for packaged queries.
#' While that allows for more complete documentation of each setting, or more
#' complex computation of values, `init_session()` is often handier for more
#' individual use cases.
#'
#' For more information on the meaining of various configuration items, see the
#' Configuration vignette.
#'
#' @param db_src A database connection created by [DBI::dbConnect()] or a
#'   similar mechanism.  If the value is a string, then [srcr::srcr()] is called
#'   is called with the string as its single parameter to create a connection.
#' @param cdm_schema The name of the database schema in which to find CDM
#'   fact tables
#' @param vocabulary_schema The name of the database schema in which to find
#'   CDM vocabulary tables
#' @param results_schema The name of the database schema into where results
#'   tables are stored
#' @param base_dir The top-level directory for this session, which must exist
#' @param subdirs A named list of subdirectory paths
#' @param cdm The type of CDM to use; one of `OMOP`, `PEDSnet`, or `PCORnet`.
#' @param table_case The case used in the database for CDM table names
#' @param table_names A mapping from (lowercase) CDM table names to actual
#'   CDM table names in the database.
#' @param results_name_tag A string to be appended to the name of any results
#'   table when it is created.
#' @param local_name_tag A string to be appended to any results table designated
#'   as `local` (e.g. in [output_tbl()]).
#' @param results_target The default destination to which to write results
#'   (database or file).
#' @param retain_intermediates A Boolean value indicating whether temporary
#'   intermediate tables should be retained after the run.
#' @param db_trace A Boolean value indicating whether commands sent to the
#'   database should be echoed into the log.
#' @param cache_enabled A Boolean value indicating whether loaded codesets
#'   should be cached and reused if another attempt is made to load that
#'   codeset.
#' @param execution_mode A flag indicating what level of automation should be
#'   applied to the run: `development` (code is still under development; little
#'   is done automatically), `production` (assume stable code; direct output to
#'   the database) or `distribution` (asume stable code; direct output to files)
#' @param ... Other named parameters used to set configuration items.  Any
#'   parameter that is not named is dropped with a warning.
#' @param .use_db A Boolean value indicating whether the session will be used
#'   with a database.  If FALSE, no checking of `db_src` is done.
#'
#' @returns The session object
#' @export
#'
#' @examples
#' \dontrun{
#'   argos$new('Prd')$init_session(db_src = 'prd_srcr')
#'
#'   argos$new('Test')$init_session(db_src = srcr::srcr('test_config_pcornet',
#'                                                      .dirs = '.'),
#'                                  base_dir = ',',
#'                                  cdm = 'pcornet', table_case = 'upper',
#'                                  results_schema = 'test_results',
#'                                  results_name_tag = '_pcor',
#'                                  retain_intermediates = TRUE)
#' }
#' @md
init_session <-
  function(db_src = NA,
           cdm_schema = NA, vocabulary_schema = NA, results_schema = NA,
           base_dir = .intuit_base_dir(), subdirs = .intuit_subdirs(base_dir),
           cdm = 'OMOP', table_case = 'lower',
           table_names = .intuit_table_names(cdm, table_case),
           results_name_tag = paste0('_',paste0(sample(letters, 5),
                                                collapse = '')),
           local_name_tag = '_loc', results_target = TRUE,
           retain_intermediates = FALSE, db_trace = TRUE, cache_enabled = TRUE,
           execution_mode = 'development',
           ..., .use_db = TRUE)
  get_argos_default()$init_session(db_src, cdm_schema, vocabulary_schema,
                                   results_schema, base_dir, subdirs, cdm,
                                   table_case, table_names, results_name_tag,
                                   local_name_tag, retain_intermediates,
                                   db_trace, cache_enabled, ..., .use_db)

argos$set(
  'public', 'init_session',
  #' @name init_session-method
  #' @inherit init_session
  function(db_src = NA,
           cdm_schema = NA, vocabulary_schema = NA, results_schema = NA,
           base_dir = .intuit_base_dir(), subdirs = .intuit_subdirs(base_dir),
           cdm = 'OMOP', table_case = 'lower',
           table_names = .intuit_table_names(cdm, table_case),
           results_name_tag = paste0('_',paste0(sample(letters, 5),
                                                collapse = '')),
           local_name_tag = '_loc', results_target = TRUE,
           retain_intermediates = FALSE, db_trace = TRUE, cache_enabled = TRUE,
           execution_mode = 'development',
           ..., .use_db = TRUE) {
    if (.use_db) {
      if (typeof(db_src) == 'character') db_src <- srcr::srcr(db_src)
      if (! is.object(db_src) ||
          # duck type for DBI
          ! any(attr(methods(class = class(db_src)), 'info')$generic
                == 'dbGetQuery'))
        cli::cli_abort('{.var db_src} must be a DBI connection')
    }
    if (! file.exists(base_dir))
      cli::cli_abort('{.var base_dir} must point to an existing directory')

    args <- c(as.list(environment()),
              rlang::dots_list(..., .homonyms. = 'last'))
    purrr::walk2(names(args), args,
                 \(k,v) {
                   if (nchar(k) == 0) {
                     cli::cli_warn('Unnamed argument {.arg v} dropped')
                   } else {
                     self$config(k,v)
                   }
                 })
    private$.setup_pkgs()
    private$.env_setup()
    self
  })

#' Set up a working argos session using a config file
#'
#' This function is a close analog of [init_session()], except that
#' configuration data are taken from a configuration file rather than arguments.
#' By default, the contents of the `argos_config` section from the configuration
#' file are passed to [init_session()] to configure the `argos` session.
#'
#' As a special case, the `db_src` configuration item is by default constructed
#' by handing the entire contents of the configuration file to [srcr::srcr()]
#' and using the returned connection as the session's `db_src`.
#'
#' @param ... Values passed to [srcr::find_config_files()] to locate potential
#'   configuration files.  The first file found that has an 'argos_config'
#'   section is used.
#' @param .read_fn The function used to read and parse the contents of the
#'   configuration file.
#' @param .use_srcr A Boolean value indicating whether [srcr::srcr()] should be
#'   called to create a database connection for `db_src`.  If `FALSE`, no
#'   other attempt is made to create the connection (but see [init_session()]).
#' @param .conf_key The configuration file section name to use, in case
#'   `argos_config` isn't the right choice for you.
#'
#' @returns The session object.
#' @export
#'
#' @examples
#' \dontrun{
#'   argos$new('StudyA')$init_session_from_config('studya_config')
#'
#'   argos$new('StudyB')$init_session_from_config(
#'     basenames = 'allstudy_setup',
#'     dirs = c(Sys.getenv('STUDYB_ROOT'), '.'),
#'     suffices = '.yaml',
#'     .read_fn = yaml::read_yaml,
#'     .conf_key = 'studyb_config')
#' }
#' @md
init_session_from_config <- function(...,
                                     .read_fn = jsonlite::fromJSON,
                                     .use_srcr = TRUE,
                                     .conf_key = 'argos_config')
  get_argos_default()$init_session_from_config(..., .read_fn, .use_srcr,
                                               .conf_key)

argos$set(
  'public', 'init_session_from_config',
  #' @name init_session_from_config-method
  #' @inherit init_session_from_config
  function(...,
           .read_fn = jsonlite::fromJSON,
           .use_srcr = TRUE,
           .conf_key = 'argos_config')
  {
    cand <- srcr::find_config_files(...)
    if (length(cand) < 1)
      cli::abort("No config paths found at {.var {as.list(...)}}")

    for (p in paths) {
      config <- tryCatch(do.call(.read_fn, list(p)), error = function(e) NA)
      if (!is.na(config[1])) {
        if (! exists(.conf_key, config)) next
        conf <- config[[.conf_key]]
        if (.use_srcr) conf[['db_src']] <- do.call(srcr::srcr, config)
        return(rlang::inject(self$init_session(!!!conf)))
      }
    }

    cli::abort("No valid config files found in {.path {paths}}")
  })
