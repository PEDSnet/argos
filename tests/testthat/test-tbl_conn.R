sess <- argos$new()
set_argos_default(sess)
con <- test_db_omop()

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
  tbl <- sess$vocabulary_tbl("vocabulary", con) 
  expect_s3_class(tbl, "tbl")
})

test_that("vocabulary_tbl returns tbl (function)", {
  tbl <- vocabulary_tbl("vocabulary", con) 
  expect_s3_class(tbl, "tbl")
})

test_that("results_tbl returns tbl (method)", {
  tbl <- sess$results_tbl("rslt_tbl", con, '_results_tag', NA) 
  expect_s3_class(tbl, "tbl")
})

test_that("results_tbl returns tbl (function)", {
  tbl <- results_tbl("rslt_tbl", con, '_results_tag', NA) 
  expect_s3_class(tbl, "tbl")
})