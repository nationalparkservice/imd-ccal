test_that("range_to_vector works", {
  expect_equal(range_to_vector("1-5"), "1 2 3 4 5")
  expect_equal(range_to_vector("2 - 3"), "2 3")
  expect_equal(range_to_vector("5-3"), "5 4 3")
  expect_equal(range_to_vector("3 1  4-6"), "3 1 4 5 6")
  expect_equal(range_to_vector("2-2"), "2")
})

test_that("range_to_vector throws error for negative numbers and non-numeric characters", {
  expect_error(range_to_vector("1 2 - -9"))
  expect_error(range_to_vector("1a 4 6 10"))
})
