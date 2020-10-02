# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
# 
# test_that("input string is correct", {
#   is.character(fetch(date_str))
# })

test_that("Wrong input throws an error.", {
  expect_error(fetch("2000-02-99"))
  expect_error(fetch("01-01-2000"))
  expect_error(fetch("2005-2020"))
  expect_error(fetch("20000110"))
  expect_error(fetch("0000-00-00"))
  expect_error(fetch("Hello"))
  expect_error(fetch(2000-01-01))
})
