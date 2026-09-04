sess_priv <- argos$new()
set_argos_default(sess_priv)
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

test_that('gen_xwalk works', {

  expect_no_error(gen_xwalk(input = cdm_tbl('person'),
                            id_col = 'person_id'))

})

test_that('new_id works', {

  xwalk_tbl <- gen_xwalk(input = cdm_tbl('person'),
                         id_col = 'person_id')

  expect_no_error(new_id(data = cdm_tbl('person'),
                         id_col = 'person_id',
                         xwalk = xwalk_tbl,
                         replace = TRUE))

})

test_that('dates_to_ages works', {

  cohort <- cdm_tbl('condition_occurrence') %>%
    select(person_id, condition_start_date)

  expect_no_error(dates_to_ages(cohort = cohort))

})

test_that('scrub_person_info', {

  cht <- cdm_tbl('condition_occurrence') %>%
    select(person_id)

  expect_no_error(scrub_person_info(cohort = cht))
})
