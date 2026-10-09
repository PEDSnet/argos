
test_that('intuit_base_dir', {

  Sys.setenv('ARGOS_DATA_REQUEST_ROOT' = file.path(getwd(), 'testfiles'))

  expect_no_error(.intuit_base_dir())

})

test_that('intuit_subdirs', {

  Sys.setenv('ARGOS_DATA_REQUEST_ROOT' = file.path(getwd(), 'testfiles'))

  expect_no_error(.intuit_subdirs(base = Sys.getenv('ARGOS_DATA_REQUEST_ROOT')))

})

test_that('intuit_table_names', {

  expect_no_error(.intuit_table_names(cdm_type = 'omop',
                                      case = 'lower'))

  expect_no_error(.intuit_table_names(cdm_type = 'pcornet',
                                      case = 'upper'))

  expect_no_error(.intuit_table_names(cdm_type = 'pedsnet',
                                      case = 'lower'))

})


test_that('init_session', {

  Sys.setenv('ARGOS_DATA_REQUEST_ROOT' = file.path(getwd(), 'testfiles'))

  con <- dbConnect(RSQLite::SQLite(), ":memory:")

  # print(methods::is(con, 'DBIConnection'))

  expect_no_error(

    argos$new('test')$init_session(db_src = dbplyr::simulate_dbi(),
                                   cdm_schema = 'my_cdm_schema',
                                   results_schema = 'my_schema',
                                   vocabulary_schema = 'my_vocab',
                                   db_trace = TRUE)

  )

})

