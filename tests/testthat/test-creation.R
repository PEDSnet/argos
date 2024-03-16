test_that('Session creation', {
  expect_null(argos::get_argos_default())

  sess <- argos$new(name = 'test creation')
  expect_setequal(class(sess), c('argos', 'R6'))

  expect_identical(argos::get_argos_default(), sess)
})

test_that('Retrieve default session', {
  expect_equal(argos::get_argos_default()$name, 'test creation')
})

test_that('Update default session', {
  sess <- argos$new(name = 'test default')
  argos::set_argos_default(sess)

  expect_identical(argos::get_argos_default(), sess)
})

test_that('Remove default session', {
  argos::set_argos_default(NULL)
  expect_null(argos::get_argos_default())
})

test_that('Default session automatically reset', {
  sess <- argos$new(name = 'test default')
  expect_identical(argos::get_argos_default(), sess)
})

