
sess_misc <- argos$new()
set_argos_default(sess_misc)
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

test_that('make_tree creates necessary directories', {

  config('base_dir', getwd())

  expect_no_error(make_tree(dirs = 'test_code'))

  unlink('test_code', recursive = TRUE)

})

test_that('add_site creates site column when site col is not present', {

  tbl <- cdm_tbl('person') %>% dplyr::collect() %>%
    select(-site)

  tbl2 <- add_site(tbl)

  expect_equal(select(tbl2, person_id, site),
               select(cdm_tbl('person') %>% dplyr::collect(), person_id, site))

})

test_that('add_site creates site column when site col is present', {

  tbl <- cdm_tbl('person') %>% dplyr::collect()

  tbl2 <- add_site(tbl)

  expect_equal(tbl2, tbl)

})

test_that('add_site creates site column when site_tbl missing site col', {

  config('qry_site', NULL)

  tbl <- cdm_tbl('person') %>% dplyr::collect() %>% select(-site)

  tbl2 <- add_site(tbl, site_tbl = tbl)

  expect_equal(tbl2, tbl %>% dplyr::mutate(site = 'unknown'))

})

test_that('lookback_facts works without specifying event_date_col',{

  cht <- cdm_tbl('condition_occurrence') %>%
    select(person_id, condition_start_date)

  expect_no_error(lookback_facts(data = cdm_tbl('drug_exposure'),
                  index_tbl = cht,
                  index_date_col = 'condition_start_date'))
})

test_that('lookback_facts works with specifying event_date_col',{

  cht <- cdm_tbl('condition_occurrence') %>%
    select(person_id, condition_start_date)

  expect_no_error(lookback_facts(data = cdm_tbl('drug_exposure'),
                                 event_date_col = 'drug_exposure_start_date',
                                 index_tbl = cht,
                                 index_date_col = 'condition_start_date'))
})
