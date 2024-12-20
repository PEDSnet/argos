sess_cds <- argos$new()
set_argos_default(sess_cds)
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
config('base_dir', getwd())
config('subdirs', list('spec_dir' = 'testdata'))

test_that('load_codeset works', {
  config('cache_enabled', TRUE)

  expect_no_error(load_codeset('test_codeset'))

})

test_that('read_codeset works', {
  config('cache_enabled', TRUE)

  expect_no_error(read_codeset('test_codeset'))

})

test_that('get_descendants works', {
  config('cache_enabled', TRUE)

  cdst <- load_codeset('test_codeset')

  expect_no_error(get_descendants(cdst, table_name = NULL))

})
