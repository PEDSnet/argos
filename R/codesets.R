#' Read a codeset from a CSV file
#'
#' This function uses the codeset name to build a path to a CSV file
#' in the data request package containing the codeset data.
#'
#' The default expects the file to be in the spec_dir directory, to have a
#' header, and to contain four columns:
#'
#'  * concept_id - integer
#'  * concept_name - string
#'  * concept_code - string
#'  * vocabulary_id - string
#'
#' Note that the codeset is re-read with each call; no caching is done.
#'
#' @param name The name of the codeset.  Typically just the file name
#'   without `.csv` suffix, but if for some reason you've expanded the
#'   directory tree under `specs`, you may prefix it with
#'   subdirectories.
#' @param col_types A column specification compatible with
#'   [readr::read_csv()].
#' @param full_path A Boolean value indicating whether `name` represents a full
#'   path or a relative file name that should be expanded.
#'
#' @return A local tbl containing the codeset
#' @export
#' @examples
#'\dontrun{
#' # Read a standard-format codeset from specs/my_study_dx.csv
#' dx_codes <- read_codeset('my_study_dx')
#' dx_codes %>% pull(concept_name)
#'
#' # Read a codeset with extra drug_class and drug_cost columns
#' # from another directory
#' rx_codes <- read_codeset('/my/project/library/meds/asthma_rx.csv',
#'                          col_types = 'iccccn', full_path = TRUE)
#' rx_codes %>% group_by(drug_class) %>% arrange(drug_cost) %>%
#'   slice_n
#'}
#' @md
read_codeset <- function(name,
                         col_types = 'iccc',
                         full_path = FALSE)
  get_argos_default()$read_codeset(name, col_types, full_path)

argos$set(
  'public', 'read_codeset',
  #' @name read_codeset-method
  #' @inherit read_codeset
  function(name, col_types = 'iccc', full_path = FALSE) {
    path <-
      if_else(full_path, name,
              file.path(self$config('base_dir'),
                        self$config('subdirs')$spec_dir,
                        paste0(name, '.csv')))
    read_csv(path, col_names = TRUE, col_types = col_types)
  })

#' Create a db table with a codeset from a CSV file
#'
#' Reads the named codeset from a CSV file, and creates a like-named
#' intermediate table in the database with the contents.
#'
#' You will typically want to construct an index on the principal code column,
#' and if the codeset is very large, columns you will use to subset it
#' during use.
#'
#' Once a codeset is loaded, it is cached by name, and future calls to
#' load_codeset() with the same name will return the cached table.
#'
#' @inheritParams read_codeset
#' @param table_name An optional name for the table, if you want it to
#'   differ from `name`.
#' @param indexes A list of columns on which indexes should be created.
#' @param db A connection to the database into which to load the codeset.
#'
#' @return A tbl pointing to the table in the database
#' @export
#' @examples
#' \dontrun{
#' meds <- load_codeset('proj_meds')
#' cdm_tbl('drug_exposure') %>%
#'   semi_join(meds, by = c('drug_concept_id' = 'concept_id'))
#'
#' # Use customized structure that supports subset analyses
#' procs <- load_codeset('/proj/shared/codesets/test_procedures.csv',
#'                       col_types = 'icccc'),
#'                       table_name = 'test_proc_classes',
#'                       indexes = list('concept_id', 'procedure_class')m
#'                       full_path = TRUE)
#' cohort_procedures %>%
#'   inner_join(procs %>% filter(procedure_class %in% c('std', 'exp')),
#'              by = c('procedure_concept_id' = 'concept_id')) %>%
#'   group_by(procedure_class) %>% count()
#' }
#' @md
load_codeset <- function(name,
                         col_types = 'iccc',
                         table_name = name,
                         indexes = list('concept_id'),
                         full_path = FALSE,
                         db = config('db_src'))
  get_argos_default()$load_codeset(name, col_types, table_name, indexes,
                                   full_path, db)

argos$set(
  'public', 'load_codeset',
  #' @name load_codeset-method
  #' @inherit load_codeset
  function(name, col_types = 'iccc', table_name = name,
           undexes = list('concept_id'), full_path = FALSE,
           db = self$config('db_src')) {

    if (self$config('cache_enabled')) {
      if (is.null(self$config('_codesets'))) self$config('_codesets', list())
      cache <- self$config('_codesets')
      if (! is.null(cache[[name]])) return(cache[[name]])
    }
    codes <-
      self$copy_to_new(db,
                       self$read_codeset(name, col_types = col_types,
                                         full_path = full_path),
                       name = table_name,
                       overwrite = TRUE,
                       indexes = indexes)

    if (self$config('cache_enabled')) {
      cache[[name]] <- codes
      self$config('_codesets', cache)
    }

    codes
  })


#' Find descendants of a codeset's elements
#'
#' Given a codeset in the database with (presumably standard) codes listed in
#' `concept_id`, create a new codeset with descendants.  By default, the new
#' codeset takes its name from the original, with `_exp` added.
#'
#' The expansion is done using the `concept_ancestor` table in the vocabulary
#' scheme, which is presumed to follow the structure for the OHDSI table of
#' that name.  If this table doesn't exist, an error will be thrown, and if
#' it doesn't have the expected structure, results are undefined.  In practice,
#' you may find the OHDSI vocabularies useful even if your data aren't in the
#' OMOP CDM, particularly because of the ontology structures and cross-
#' terminology relationships they capture.
#'
#' Note that this function is intended primarily for expanding a codeset
#' containing high-level terms during query execution.  For a similar method
#' better suited to construction of codesets to be saved for later use, see
#' [expand_codeset()] in `locode/build_concepts.R`. (FIXME)
#'
#' @param codeset A database tbl containing the starting codeset
#' @param table_name The name to give the expanded codeset, if you're not happy
#'   with the default.
#'
#' @return A database tbl containing the expanded codeset, with the standard
#'   4 columns (see above), indexed on `concept_id`.
#' @export
#' @examples
#' \dontrun{
#' snomed_subtree <- get_descendants(snomed_roots)
#' }
#' @md
get_descendants <- function(codeset, table_name = NA)
  get_argos_default()$get_descendants(codeset, table_name)

argos$set(
  'public', 'get_descendants',
  #' @name get_descendants-method
  #' @inherit get_descendants
  function(codeset, table_name = NA) {
    if (is.na(table_name)) {
      table_name <- dbplyr:::tbl_desc(codeset)
      table_name <- unlist(regmatches(table_name,
                                      regexec('(\\w+)>', table_name,
                                              perl = TRUE)))[2]
      table_name <- paste0(table_name, '_exp')
    }

    if (is.null(self$config('_codesets'))) self$config('_codesets', list())
    cache <- self$config('_codesets')
    if (! is.null(cache[[table_name]])) return(cache[[table_name]])

    codes <- self$vocabulary_tbl('concept_ancestor') %>%
      inner_join(codeset, by = c('ancestor_concept_id' = 'concept_id')) %>%
      select(descendant_concept_id) %>%
      inner_join(self$vocabulary_tbl('concept'),
                 by = c('descendant_concept_id' = 'concept_id')) %>%
      select(concept_id = descendant_concept_id, concept_name, concept_code,
             vocabulary_id) %>%
      distinct() %>%
      self$compute_new(indexes = list('concept_id'), name = table_name)
    cache[[table_name]] <- codes
    self$config('_codesets', cache)
    codes
  })
