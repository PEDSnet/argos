#' Class encapsulating an argos session
#'
#' `Argos` is built around the concept of a _session_, that collects information
#' about database configuration and connection, and options for executing a query.
#'
#' To use `argos`, you need to create at least one session.  After that, `argos`
#' provides both a functional and object-oriented interface to its services, so
#' if it fits your working style better, you can have multiple active sessions
#' and operate on each via method calls on each session object.
#'
#' @export
argos <-
  R6::R6Class('argos',
          public = list(
            #' @field name A string with a name or short description
            name = NA_character_,
            #' @description Set up an `argos` session
            #' @param name The session name
            #' @return The `argos` session
            initialize = function(name = NA) {
              self$name <- name
              private$req_env <- new.env(parent = emptyenv())
              if (is.null(get_argos_default())) set_argos_default(self)
            }
          ),
          private = list('req_env' = NULL)
  )

#' Get the default `argos` session
#'
#' When you use the functional interface (that is, call `argos` functions as
#' freestanding invocations rather than method calls on an explicit object),
#' then the default session is used as the context for the call, supplying any
#' needed state.
#'
#' The default session can be set in one of two ways.  When you create an
#' `argos` session, if there is no current default session, it will
#' automatically be registered as the default until you replace it or exit the
#' R interpreter.  Subsequently, you may use [set_argos_default()] to change the
#' default session.
#'
#' @return The session object.
#' @examples
#' get_argos_default()
#' @export
#' @md
get_argos_default <- function() .pkg_env$defobj

#' Set the default `argos` session
#'
#' This sets the default session to a new session object.  All calls via the
#' functional interface will subsequently use the new default session.  Note,
#' however, that no state from the prior default session is automatically
#' transferred to the new one, so be careful about operations in progress
#' when the default session is changed.  Better still, only change the default
#' session when you can make a clean break from previous activity.
#'
#' If you set the default session to `NULL`, then the next session created will
#' become the default.  If you want there to be no default session from this
#' point onward, then set the default session to `NA`, and it will not be
#' overridden unless you do so manually.
#'
#' If there is no default session, calls using the functional interface will
#'  fail with an error.
#'
#' @param session An `argos` session object, NA, or NULL.
#'
#' @return The session object.
#' @examples
#' \dontrun{argos$new(name = 'Default', ...) |> set_argos_default()}
#' @export
#' @md
set_argos_default <- function(session) {
  if ( !(any(class(session) == 'argos') ||
         all(is.null(session)) || all(is.na(session))) )
    stop('Argument to set_argos_default() must be argos session, NA, or NULL')
  .pkg_env$defobj <- session
}
