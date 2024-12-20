
test_that('setup', {

  sess_shims <- argos$new()
  set_argos_default(sess_shims)
  con <- mk_testdb_omop()
  config('db_src', con)
  config('base_dir', file.path(getwd(), 'testfiles'))
  config('execution_mode', 'distribution')
  config('subdirs', list('code_dir' = 'code',
                         'result_dir' = 'results'))
  config('site_info', paste0(config('base_dir'), '/code/driver.R'))
  config('retain_intermediates', FALSE)
  config('results_schema', NA)
  config('extra_packages', c('tidyr'))

  # base_dir <- config('base_dir')

  expect_no_error(setup(here = config('base_dir'),
                     driver = 'driver.R'))

})


test_that('run_request', {

  sess_shims <- argos$new()
  set_argos_default(sess_shims)
  con <- mk_testdb_omop()
  config('db_src', con)
  config('base_dir', file.path(getwd(), 'testfiles'))
  config('execution_mode', 'distribution')
  config('subdirs', list('code_dir' = 'code',
                         'result_dir' = 'results'))
  config('site_info', paste0(config('base_dir'), '/code/driver.R'))
  config('retain_intermediates', FALSE)
  config('results_schema', NA)
  config('extra_packages', c('tidyr'))

  # base_dir <- config('base_dir')

  expect_no_error(run_request(base_dir = config('base_dir'),
                              driver = 'driver.R'))

})
