test_that("it runs without error", {
  expect_no_condition(ggcorrhm(mtcars))
  expect_no_condition(ggcorrhm(mtcars, bins = 5, cell_shape = 21))
  # Two input matrices
  expect_no_condition(ggcorrhm(mtcars, iris[1:32, -5], return_data = T, label_cor = T))
  # P-values
  expect_no_condition(ggcorrhm(mtcars, p_calc = T, p_adj = "fdr", p_thr = 0.05, return_data = T))
  expect_no_condition(ggcorrhm(mtcars, p_calc = T, p_adj = "fdr", p_thr = 0.05, return_data = F,
                               label_cor = T))
  expect_no_condition(ggcorrhm(mtcars, iris[1:32, -5], p_calc = T))
})

