
sess_io <- argos$new()
set_argos_default(sess_io)
con <- mk_testdb_omop()
config('db_src', con)
config('cdm_schema', NA)
config('results_schema', NA)
config('results_name_tag', '')
config('local_name_tag', '_loc')
config('table_names', list('person' = 'person'))
config('vocabulary_schema', NA)
config('retain_intermediates', FALSE)
config('db_trace', TRUE)
config('can_explain', TRUE)

test_that('compute_new works for remote table', {

  tbl <- cdm_tbl('person') %>% compute_new()

  expect_s3_class(tbl, 'tbl')

})

test_that('compute_new does NOT work for local table', {

  tbl <- cdm_tbl('person') %>% dplyr::collect()

  expect_error(tbl %>% compute_new())

})

test_that('collect_new works for remote table', {

  tbl <- cdm_tbl('person') %>% collect_new()

  expect_s3_class(tbl, 'tbl')

})

test_that('collect_new works for local table', {

  tbl <- cdm_tbl('person') %>% dplyr::collect()

  expect_no_error(tbl %>% collect_new())

})

test_that('copy_to_new works for local table', {

  tbl <- cdm_tbl('person') %>% dplyr::collect()

  expect_no_error(copy_to_new(df = tbl))

})

test_that('copy_to_new does NOT work for remote table', {

  tbl <- cdm_tbl('person') #%>% compute_new()

  expect_no_error(copy_to_new(df = tbl))

})

test_that('output_tbl outputs a local table', {

  config('results_target', TRUE)

  tbl <- cdm_tbl('person') %>% dplyr::collect()

  output_tbl(tbl, 'test_output')

  expect_s3_class(results_tbl('test_output', con), 'tbl')

})

test_that('output_tbl outputs a remote table', {

  config('results_target', TRUE)

  tbl <- cdm_tbl('person')

  output_tbl(tbl)

  expect_s3_class(results_tbl('tbl', con), 'tbl')

})


test_that('output_tbl outputs to a file', {

  config('base_dir', getwd())
  config('subdirs', list('result_dir' = 'testdata'))

  tbl <- cdm_tbl('person')

  expect_no_error(output_tbl(tbl, file = TRUE))

  #csv_tbl <- readr::read_csv(file.path(config('base_dir'), config('subdirs')$result_dir, 'tbl.csv'))

  #expect_s3_class(csv_tbl, 'tbl')

})
