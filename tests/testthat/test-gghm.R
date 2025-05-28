test_that("basic functionality works", {
  expect_no_error(gghm(mtcars))
  # Different dimensions
  expect_no_error(gghm(mtcars[1:2, ]))
  expect_no_error(gghm(mtcars[, 1:2]))
  expect_no_error(gghm(mtcars[1:2, 1:2]))
})

test_that("correct input types", {
  expect_error(gghm("a"), "x must be a matrix or data frame")
  expect_error(gghm(1), "x must be a m")
})

test_that("warnings for layouts and clustering", {
  expect_warning(gghm(mtcars, layout = "br"), "A triangular layout with an a")
  expect_warning(gghm(cor(mtcars), layout = "br", cluster_rows = T), "Cannot cluster only one")
})
