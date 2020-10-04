context("apiFetch")

test_that("Wrong input throws an error.", {
  expect_error(fetch(date_str = "2000-02-99"))
  expect_error(fetch(date_str = "01-01-2000"))
  expect_error(fetch(date_str = "2005-2020"))
  expect_error(fetch(date_str = "20000110"))
  expect_error(fetch(date_str = "0000-00-00"))
  expect_error(fetch(date_str = "Hello"))
  expect_error(fetch(date_str = 2000-01-01))
  expect_error(fetch(url = 2))
  expect_error(fetch(url = "10.4.2.5.6.2"))
  expect_error(fetch(contains_date = "character input"))
})

test_that("Strange URL gives warnings.", {
  expect_error(fetch(url = ":https://10.5. 1.6"))
})

test_that("Output is proper.", {
  expect_output(print(fetch()[1]), "\\$data\\nSimple( )feature( )collection( )with( )", all = FALSE, fixed = FALSE)
})

test_that("Output is list.", {
  expect_type(fetch(), "list")
})