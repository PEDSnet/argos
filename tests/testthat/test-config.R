# Make sure we know default session state
sess <- argos$new()
set_argos_default(sess)
sess2 <- argos$new()

test_that('Config exists is false for nonexistent element (method)', {
  expect_false(sess$config_exists('test_elt'))
})
test_that('Config exists is false for nonexistent element (function)', {
  expect_false(config_exists('test_elt'))
})

test_that('Config element can be set (method)', {
  expect_equal(sess$config('test_elt', 'valuable'), 'valuable')
})

test_that('Config element can be set (function)', {
  expect_equal(config('test_elt', 'valued'), 'valued')
})

test_that('Config element can be retrieved (method)', {
  expect_equal(sess$config('test_elt'), 'valued')
})

test_that('Config element can be retrieved (function)', {
  expect_equal(config('test_elt'), 'valued')
})

test_that('Config setting does not cross objects',{
  expect_null(sess2$config('test_elt'))
})

test_that('Config append works (method)', {
  expect_equal(sess$config_append('test_elt', c('a', 'b')),
               c('valued', 'a', 'b'))
})

test_that('Config append works (function)', {
  expect_equal(config_append('test_elt', c('c', 'd')),
               c('valued', 'a', 'b', 'c', 'd'))
})

test_that('Config element can be removed (method)', {
  sess$config_rm('test_elt')
  expect_null(sess$config('test_elt'))
})

test_that('Config element can be removed (function)', {
  sess$config('test_elt', 'present')
  config_rm('test_elt')
  expect_null(sess$config('test_elt'))
})

chm <- sess$config_handle()

test_that('Config handle can be retrieved (method)', {
  expect_identical(class(chm), c('_co_req'))
})

chf <- config_handle()

test_that('Config handle can be retrieved (function)', {
  expect_identical(class(chf), c('_co_req'))
  expect_identical(chm, chf)
})

test_that('Config elements can be retrieved via handle', {
  config('test_helt', 'valued')
    expect_identical(chm[['test_helt']], 'valued')
  expect_identical(chf$test_helt, 'valued')
})

test_that('Config elements can be set via handle', {
  chm[['test_bracket']] <- 'success'
  expect_identical(chf[['test_bracket']], 'success')
  chm$test_dollar <- 'succeeded'
  expect_identical(chf$test_dollar, 'succeeded')
})

