
sess_step <- argos$new()
set_argos_default(sess_step)
con <- mk_testdb_omop()

test_that('init_sum creates step log', {

  expect_no_error(init_sum('value' = 7,
                           qry_site = 'test'))

})

test_that('append_sum adds to step log', {

  expect_no_error(append_sum('value' = 11,
                             qry_site = 'test2'))

})

test_that('distinct_ct works', {

  config('cdm_schema', NA)

  expect_no_error(append_sum('value' = cdm_tbl('person', con) %>% distinct_ct,
                             qry_site = 'test3'))

})

test_that('output_sum outputs the step log', {

  config('results_target', TRUE)
  config('results_name_tag', '')
  config('results_schema', NA)
  config('db_trace', FALSE)

  output_sum(name = 'test_steps', db = con)

  expect_s3_class(results_tbl('test_steps', db = con), 'tbl')

})

