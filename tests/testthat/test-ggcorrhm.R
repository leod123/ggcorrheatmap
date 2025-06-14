test_that("it runs without error", {
  expect_no_error(ggcorrhm(mtcars))
  expect_no_error(ggcorrhm(mtcars, bins = 5, cell_shape = 21))
  # Two input matrices
  expect_no_error(ggcorrhm(mtcars, iris[1:32, -5], return_data = T, label_cor = T))
  # P-values
  expect_no_error(ggcorrhm(mtcars, p_calc = T, p_adj = "fdr", p_thr = 0.05, return_data = T))
  expect_no_error(ggcorrhm(mtcars, p_calc = T, p_adj = "fdr", p_thr = 0.05, return_data = F,
                           label_cor = T))
  expect_no_error(ggcorrhm(mtcars, iris[1:32, -5], p_calc = T))
})

test_that("snapshots are ok", {
  vdiffr::expect_doppelganger("basic_ggcorrhm", ggcorrhm(mtcars))
  vdiffr::expect_doppelganger("asymmetric_corrhm", ggcorrhm(iris[1:32, -5], mtcars))
  vdiffr::expect_doppelganger("corr_layout", ggcorrhm(mtcars, layout = "tr"))
  vdiffr::expect_doppelganger("corr_w_options", ggcorrhm(mtcars, cluster_rows = T, cluster_cols = T,
                                                         annot_rows_df = data.frame(.names = colnames(mtcars), a = 1:ncol(mtcars)),
                                                         annot_cols_df = data.frame(.names = colnames(mtcars), b = 1:ncol(mtcars))))
  vdiffr::expect_doppelganger("cell_shape", ggcorrhm(mtcars, cell_shape = 21))
  vdiffr::expect_doppelganger("pvalues", ggcorrhm(mtcars, p_calc = T, p_thr = 0.05, p_adj = "fdr"))
})
