# Generate a test DB with synthetic omop data
testdb <- NULL
# Function to generate omop test db
mk_testdb_omop <- function(){
  if (! is.null(testdb)) return(testdb)

  # Create an ephemeral in-memory RSQLite database
  testdb <<- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # Read all csv files in testdata folder
  col_type_mapping <- list(
    'care_site' = 'iciccc',
    'condition_occurrence' = 'iiiDDiciici',
    'drug_exposure' = 'iiiDDicddicidiciicicc',
    'fact_relationship' = 'iiiii',
    'location' = 'iccccccc',
    'observation' = 'iiiDTiidiiddiccicc',
    'observation_period' = 'iiDDi',
    'person' = 'ciiiiiTiiiiicciciciDD',
    'procedure_occurrence' = 'iiiDiidiicic',
    'provider' = 'iccciiiiccici',
    'visit_occurrence' = 'iiiDtDtiiici',
    'concept_ancestor' = 'iicc',
    'concept' = 'icccc')
  for (file_name in list.files(path='testdata', pattern = "\\.csv$")) {
    # Get table_name from csv file_name without extension
    table_name <- sub('\\.csv$', '', file_name)
    tbl <- read_csv(
      file = file.path('testdata', file_name),
      col_types = col_type_mapping[[table_name]],
      show_col_types = FALSE
    )
    # Write to db
    dbWriteTable(testdb, table_name, tbl)
  }
  testdb
}

