# Function to generate a test DB with mtcars and iris
test_db <- function(){
  # Create an ephemeral in-memory RSQLite database
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # Write mtcars and iris
  dbWriteTable(con, "mtcars", mtcars)
  dbWriteTable(con, "iris", iris)
  con
}


# Function to generate a test DB with synthetic omop data
test_db_omop <- function(){
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # TODO: load synthetic omop cdm into test database

  con
}
