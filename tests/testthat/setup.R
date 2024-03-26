# Function to generate a test DB with mtcars and iris
test_db <- function(){
  # Create an ephemeral in-memory RSQLite database
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # Write mtcars and iris
  dbWriteTable(con, "mtcars", mtcars)
  dbWriteTable(con, "iris", iris)
  con
}




# Generate a test DB with synthetic omop data

# Define column type in csv files
# TODO: Do we need accurate data type for testing? If so, add col type for each csv file below
col_type_mapping <- list()
col_type_mapping$care_site <- 'iciccc'


# Function to generate omop test db
test_db_omop <- function(){
  # Create an ephemeral in-memory RSQLite database
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # Read all csv files in testdata folder
  csv_files <- list.files(path='testdata', pattern = "\\.csv$")

  for (file_name in csv_files){
    # Get table_name from csv file_name without extension
    table_name <- sub('\\.csv$', '', file_name)
    tbl <- read_csv(
      file = file.path('testdata', file_name),
      col_types = col_type_mapping[[table_name]],
      show_col_types = FALSE
    )
    # Write to db
    dbWriteTable(con, table_name, tbl)
  }
  con
}

