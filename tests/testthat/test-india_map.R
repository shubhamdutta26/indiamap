test_that("state data frame is returned", {
  data <- india_map()

  expect_equal(ncol(data), 4)
  expect_equal(nrow(data), 37)

  expect_identical(india_map("state"), data)
  expect_identical(india_map("states"), data)
})

test_that("county data frame is returned", {
  data <- india_map("districts")

  expect_equal(ncol(data), 5)
  expect_equal(nrow(data), 755)

  expect_identical(india_map("district"), data)
})
