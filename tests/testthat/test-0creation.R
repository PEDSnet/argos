test_that('Default is clear before first session', {
  expect_null(argos::get_argos_default())
})

test_that('Session creation works', {
  sess <- argos$new(name = 'test creation')
  expect_setequal(class(sess), c('argos', 'R6'))
})

test_that('First new session sets default', {
  expect_equal(argos::get_argos_default()$name, 'test creation')
})

test_that('Retrieving default session works', {
  expect_equal(argos::get_argos_default()$name, 'test creation')
})

test_that('Updating default session works', {
  sess <- argos$new(name = 'test default')
  argos::set_argos_default(sess)

  expect_identical(argos::get_argos_default(), sess)
})

test_that('Removing default session works', {
  argos::set_argos_default(NULL)
  expect_null(argos::get_argos_default())
})

test_that('Default session automatically resets after removal', {
  sess <- argos$new(name = 'test default')
  expect_identical(argos::get_argos_default(), sess)
})

test_that('Turning off default session works', {
  argos::set_argos_default(NA)
  expect_true(is.na(argos::get_argos_default()))
})

test_that('Session errors with improper input', {
  expect_error(set_argos_default('test'))
})

test_that('Default session does not reset after turning off', {
  sess <- argos$new(name = 'test default')
  expect_true(is.na(argos::get_argos_default()))
})

