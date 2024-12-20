sess <- argos$new()
set_argos_default(sess)
con <- mk_testdb_omop()
sess$config('db_src', con)
sess$config('cdm_schema', NA)
sess$config('results_schema', NA)
sess$config('results_name_tag', '')
sess$config('local_name_tag', '_loc')
sess$config('table_names', list('person' = 'person'))
sess$config('vocabulary_schema', NA)

test_that("qual_tbl returns tbl (method)", {
  tbl <- sess$qual_tbl('person', 'cdm_schema', con)
  expect_s3_class(tbl, "tbl")
})

test_that("qual_tbl returns tbl (function)", {
  tbl <- qual_tbl("person", "cdm_schema", con)
  expect_s3_class(tbl, "tbl")
})

test_that("cdm_tbl returns tbl (method)", {
  tbl <- sess$cdm_tbl("person", con)
  expect_s3_class(tbl, "tbl")
})

test_that("cdm_tbl returns tbl (function)", {
  tbl <- cdm_tbl("person", con)
  expect_s3_class(tbl, "tbl")
})

test_that("vocabulary_tbl returns tbl (method)", {
  tbl <- sess$vocabulary_tbl("person", con)
  expect_s3_class(tbl, "tbl")
})

test_that("vocabulary_tbl returns tbl (function)", {
  tbl <- vocabulary_tbl("person", con)
  expect_s3_class(tbl, "tbl")
})

test_that("results_tbl returns tbl (method)", {
  tbl <- sess$results_tbl("person", con)
  expect_s3_class(tbl, "tbl")
})

test_that("results_tbl returns tbl (function)", {
  tbl <- results_tbl("person", con)
  expect_s3_class(tbl, "tbl")
})
