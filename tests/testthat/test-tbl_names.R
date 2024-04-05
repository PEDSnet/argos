sess <- argos$new()
# Insure known default state for functional testing
set_argos_default(sess)
con <- mk_testdb_omop()
sess$config('db_src', con)
sess$config('cdm_schema', 'test_cdm')
sess$config('results_schema', 'test_result')
sess$config('results_name_tag', '_test000')
sess$config('local_name_tag', '_loc')
sess$config('table_names', list('person' = 'personal'))

test_that('qual_name passes through fully-qualified name', {
  proband <- dbplyr::in_schema('known_schema', 'table')
  expect_equal(sess$qual_name(proband, 'cdm_schema'),
               proband)
})

test_that("qual_name handles unmapped base name (method)", {
  expect_equal(sess$qual_name('new_table', 'cdm_schema'),
               dbplyr::in_schema('test_cdm', 'new_table'))
})

test_that("qual_name handles unmapped base name (function)", {
  expect_equal(qual_name('new_table', 'cdm_schema'),
               dbplyr::in_schema('test_cdm', 'new_table'))
})

test_that("qual_name handles mapped base name  (method)", {
  expect_equal(sess$qual_name('person', 'cdm_schema'),
               dbplyr::in_schema('test_cdm', 'personal'))
})

test_that("qual_name handles mapped base name  (function)", {
  expect_equal(qual_name('person', 'cdm_schema'),
               dbplyr::in_schema('test_cdm', 'personal'))
})

test_that("intermed_name returns temporary table name with results_tag (method)", {
  expect_equal(sess$intermed_name('temp_tbl', temporary = TRUE,
                                  results_tag = '_results_tag',
                                  local_tag = NA,
                                  schema = 'results_schema'),
               'temp_tbl_results_tag')
})

test_that("intermed_name returns temporary table name with results_tag (function)", {
  expect_equal(intermed_name('temp_tbl', temporary = TRUE,
                             results_tag = '_results_tag',
                             local_tag = NA,
                             schema = 'results_schema'),
               'temp_tbl_results_tag')
})

test_that("intermed_name returns qualified table name with results_tag (method)", {
  expect_equal(sess$intermed_name('perm_tbl', temporary = FALSE,
                                  local_tag = NA,
                                  schema = 'results_schema'),
               dbplyr::in_schema('test_result',
                                 'perm_tbl_test000'))
})

test_that("intermed_name returns qualified table name with results_tag (function)", {
  expect_equal(intermed_name('perm_tbl', temporary = FALSE,
                             local_tag = NA,
                             schema = 'results_schema'),
               dbplyr::in_schema('test_result',
                                 'perm_tbl_test000'))
})

test_that("intermed_name returns qualified table name with local_tag (method)", {
  expect_equal(sess$intermed_name('perm_tbl', temporary = FALSE,
                                  local_tag = TRUE,
                                  schema = 'results_schema'),
               dbplyr::in_schema('test_result',
                                 'perm_tbl_loc_test000'))
})

test_that("intermed_name returns qualified table name with local_tag (function)", {
  expect_equal(intermed_name('perm_tbl', temporary = FALSE,
                             local_tag = TRUE,
                             schema = 'results_schema'),
               dbplyr::in_schema('test_result',
                                 'perm_tbl_loc_test000'))
})

test_that('intermed_name adds tags to fully-qualified name', {
  expect_equal(sess$intermed_name(dbplyr::in_schema('known', 'table'),
                                  temporary = FALSE, local_tag = TRUE,
                                  schema = 'results_schema'),
               dbplyr::in_schema('known', 'table_loc_test000'))
})

test_that("results_name returns qualified table name with results_tag (method)", {
  expect_equal(sess$results_name('perm_tbl', local_tag = NA),
               dbplyr::in_schema('test_result',
                                 'perm_tbl_test000'))
})

test_that("results_name returns qualified table name with results_tag (function)", {
  expect_equal(results_name('perm_tbl', local_tag = NA),
               dbplyr::in_schema('test_result',
                                 'perm_tbl_test000'))
})

test_that("results_name returns qualified table name with local_tag (method)", {
  expect_equal(sess$results_name('perm_tbl', local_tag = TRUE),
               dbplyr::in_schema('test_result',
                                 'perm_tbl_loc_test000'))
})

test_that("results_name returns qualified table name with local_tag (function)", {
  expect_equal(results_name('perm_tbl', local_tag = TRUE),
               dbplyr::in_schema('test_result',
                                 'perm_tbl_loc_test000'))
})
