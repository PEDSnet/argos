# Top-level code for execution of data request

# Load additional packages at runtime
.setup_pkgs <-function(pkgs = config('extra_packages'))
  get_argos_default()$.setup_pkgs(pkgs)

argos$set(
  'private', '.setup_pkgs',
  function(pkgs = config('extra_packages')){
    if (! all(is.null(pkgs)) && ! all(is.na(pkgs))) {
      for (pkg in pkgs)
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }
  })

#' Set up the execution environment
#'
#' This function sources the R files needed to execute the query and sets
#' up the execution environment.  In particular, all of the framework files, as
#' well as request-specific files in the code_dir with names matching
#' `cohort_*.R` or `analyze_*.R` will be sourced.
#'
#' This function is usually run automatically when the `run.R` file is sourced
#' to execute the request.  It may also be executed manually during an
#' interactive session to re-source changed code or to re-establish a connection
#' to the database.
#'
#' One principle to note is that changes to the global environment, such as
#' loading packages, will be visible to all argos objects.  If you are managing
#' multiple sessions in different objects, be careful about the potential to
#' change global state during setup.
#'
#' @param here The name of the top-level directory for the request.  The default
#'   is `config('base_dir')` if the config function has been set up, or the
#'   global variable `base_dir` if not.
#' @param driver The name of the driver file to source.  This allows you to set
#'   up alternate execution paths (.run() functions) for your project and
#'   specify which one to follow.
#'
#' @return The value of `here`.
#' @export
#' @md
setup <- function(here = base::ifelse(typeof(get('config')) == 'closure',
                                      config('base_dir'), base_dir),
                  driver = 'driver.R')
  get_argos_default()$setup(here, driver)

argos$set(
  'public', 'setup',
  #' @name setup-method
  #' @inherit setup
  function(here = base::ifelse(typeof(get('config')) == 'closure',
                               self$config('base_dir'), base_dir),
           driver = 'driver.R') {
    source(file.path(here, 'code', 'req_info.R'))
    source(config('site_info'))
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                          'cohort_.+\\.R', full.names = TRUE))
      source(fn)
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                        'analyze_.+\\.R', full.names = TRUE))
      source(fn)
    source(file.path(here, config('subdirs')$code_dir, 'cohorts.R'))
    source(file.path(here, config('subdirs')$code_dir, driver))

    private$.setup_pkgs()

    private$.env_setup()

    for (def in c('retain_intermediates', 'results_schema')) {
      if (! self$config_exists(def) || is.na(self$config(def)))
        self$config(def, self$config(paste0('default_', def)))
    }

    if (self$config_exists('db_src')) {
      ci <- DBI::dbGetInfo(self$config('db_src'))
      message('Default database connection is: ',
              ci$dbname, '@', ci$host)
    }

    here
  })


#' Set up and execute a data request
#'
#' This function encapsulates a "production" run of the data request.  It sets
#' up the environment, executes the request, and cleans up the environment.
#'
#' Typically, the `run.R` file calls run_request() when in a production mode.
#'
#' @param base_dir Path to the top of the data request files.  This is
#'   typically specified in `run.R`.
#' @param driver The name of the driver file to source (cf. [.load()]).
#'
#' @return The result of [.run()].
#' @export
#' @md
run_request <- function(base_dir, driver = 'driver.R')
  get_argos_default()$run_request(base_dir, driver)

argos$set(
  'public', 'run_request',
  #' @name run_request-method
  #' @inherit run_request
  function(base_dir, driver = 'driver.R') {
    base_dir <- self$setup(base_dir)
    on.exit(private$.env_cleanup())
    self$run()
  })
