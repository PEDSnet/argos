sess <- argos$new()
set_argos_default(sess)
con <- test_db_omop()

test_that("Returns name if base name not found in `config('table_names') (method)", {
  expect_equal(sess$qual_name('new_table', 'cdm_schema', con), 'new_table')
})

test_that("Returns name if base name not found in `config('table_names') (function)", {
  expect_equal(qual_name('new_table', 'cdm_schema', con), 'new_table')
})

test_that("Returns temporary table name with results_tag (method)", {
  expect_equal(sess$intermed_name('temp_tbl', TRUE, '_results_tag', NA, 'results_schema', con), 'temp_tbl_results_tag')
})

test_that("Returns temporary table name with results_tag (function)", {
  expect_equal(intermed_name('temp_tbl', TRUE, '_results_tag', NA, 'results_schema', con), 'temp_tbl_results_tag')
})

test_that("Returns results table name with results_tag (function)", {
  expect_equal(results_name('rslt_tbl', '_results_tag', NA, con), 'rslt_tbl_results_tag')
})