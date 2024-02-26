#' Drop a table in a database, schema-aware
#'
#' This function has the same purpose as [DBI::dbRemoveTable()], but many
#'  implementations are unable to accommodate a table name created by
#'  [dbplyr::in_schema()].
#'
#' Note that `db` is the first parameter, even though the default is almost
#' always what you wnat, in order to maintain a calling sequence consistent with
#' what you'd expect from the DBI specification.  So you'll probably want to
#' call this function as `db_remove_table( name =` _tabname_ `)` most of the time.
#'
#' @param db A [dplyr::src()] or DBI connection
#' @param name The name of the table, or an object created by
#'   [dbplyr::in_schema()], or a DBI-style path vector
#' @param tenporary Whether `table` is a temporary table
#' @param fail_if_missing If TRUE, raise an error if `table` does not exist
#'
#' @return TRUE, invisibly, if no error is encountered; an exception is raised
#'   otherwise
#' @export
#' @md
db_remove_table <- function(db = config('db_src'), name,
                            temporary = FALSE, fail_if_missing = FALSE) {
  con <- dbi_con(db)
  elts <- .parse_tblspec(name)

  sql <- paste0('drop table ',
                base::ifelse(fail_if_missing, '', 'if exists '))
  if (any(grepl('ora', class(con), ignore.case = TRUE))) {
    if (! db_exists_table(con, name)) return(TRUE)
    if (length(elts) > 1) {
    elts <- rev(elts)
   DBI::dbRemoveTable(con, elts[1], schema = elts[2])
    } else {
      DBI::dbRemoveTable(con, elts[1])
    }
  }
  else if (!temporary &&
             any(class(con) %in% c('PostgreSQLConnection', 'PqConnection')) &&
             length(elts) > 1) {
    name <- DBI::SQL(paste0(DBI::dbQuoteIdentifier(con, elts[1]),
                               '.',
                               DBI::dbQuoteIdentifier(con, elts[2])))
    DBI::dbExecute(con, paste0(sql, name))
  }
  else {
    name <- paste0(vapply(elts, function (x) DBI::dbQuoteIdentifier(con, x),
                             FUN.VALUE = character(1)), collapse = '.')
    DBI::dbExecute(con, paste0(sql, name))
  }
  invisible(TRUE)
}


#' Test whether a table exists in a database, schema-aware
#'
#' This function has the same purpose as [DBI::dbExistsTable()] or
#' [dbplyr::db_has_table()], but neither of those are able to test a
#' table name created by [dbplyr::in_schema()].
#'
#' Note that `db` is the first parameter, even though the default is almost
#' always what you wnat, in order to maintain a calling sequence consistent with
#' what you'd expect from the DBI specification.  So you'll probably want to
#' call this function as `db_exists_table( name =` _tabname_ `)` most of the time.
#'
#' @param db A [dplyr::src()] or DBI connection
#' @param name The name of the table, or an object created by
#'   [dbplyr::in_schema()], or a DBI-style path vector
#'
#' @return TRUE if the table exists, and FALSE otherwise.
#' @export
#' @md
db_exists_table <- function(db = config('db_src'), name) {
  con <- dbi_con(db)
  elts <- .parse_tblspec(name)

  if (any(grepl('ora', class(con), ignore.case = TRUE)) &&
      length(elts) > 1) {
    elts <- rev(elts)
    return(DBI::dbExistsTable(con, elts[1], schema = elts[2]))
  }
  else if (any(class(con) == 'PostgreSQLConnection') &&
           length(elts) == 1) {
    res <-
      DBI::dbGetQuery(con,
                      paste("select tablename from pg_tables where ",
                            "schemaname !='information_schema' and schemaname !='pg_catalog' ",
                            "and schemaname in (select schemas[nr] from ",
                            "(select *, generate_subscripts(schemas,1) as nr ",
                            "from (select current_schemas(true) as schemas) a ",
                            ") b where schemas[nr] <> 'pg_catalog') and tablename=",
                            DBI::dbQuoteString(con, elts[1]), sep = ""))
    return(as.logical(dim(res)[1]))
  }
  else if (any(class(con) == 'PqConnection') && length(elts) > 1) {
    return(DBI::dbExistsTable(con, DBI::Id(elts)))
  }
  else {
    return(DBI::dbExistsTable(con, elts))
  }
}
