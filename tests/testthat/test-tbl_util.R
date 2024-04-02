sess <- argos$new()
set_argos_default(sess)
con <- test_db_omop()

test_that('DBI con is NULL if `db` is not a connection (method)', {
  expect_null(sess$dbi_con('not_con'))
})

test_that('DBI con is NULL if `db` is not a connection (function)', {
  expect_null(dbi_con('not_con'))
})

test_that('Table exists is false for nonexistent table (method)', {
  expect_false(sess$db_exists_table(con, 'test_tbl'))
})

test_that('Table exists is false for nonexistent table (function)', {
  expect_false(db_exists_table(con, 'test_tbl'))
})

test_that('Drop table error for non-existent table (method)', {
  expect_error(sess$db_remove_table(con, 'test_tbl', FALSE, TRUE))
})

test_that('Drop table error for non-existent table (function)', {
  expect_error(db_remove_table(con, 'test_tbl', FALSE, TRUE))
})