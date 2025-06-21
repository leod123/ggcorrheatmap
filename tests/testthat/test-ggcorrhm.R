test_that("it runs without error", {
  expect_no_error(ggcorrhm(mtcars))
  expect_no_error(ggcorrhm(mtcars, bins = 5, mode = "21"))
  # Two input matrices
  expect_no_error(ggcorrhm(mtcars, iris[1:32, -5], return_data = T, cell_labels = T))
  # P-values
  expect_no_error(ggcorrhm(mtcars, p_values = T, p_adjust = "fdr", return_data = T))
  expect_no_error(ggcorrhm(mtcars, p_values = T, p_adjust = "fdr", return_data = F, cell_labels = T))
  expect_no_error(ggcorrhm(mtcars, iris[1:32, -5], p_values = T))
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("hm", "18")))
})

test_that("p-value errors work", {
  expect_error(ggcorrhm(mtcars, p_values = T, p_thresholds = c("a" = -1, "b" = 0.5, "c" = 1)),
               "The p-value thresholds must be above 0")
  expect_error(ggcorrhm(mtcars, p_values = T, p_thresholds = c("***" = 0.001, "**" = 0.01, "*" = 0.05, .1)),
               "The last value of 'p_thresholds' must be 1 or larger")
  expect_error(ggcorrhm(mtcars, p_values = T, p_thresholds = c(0.001, 0.01, 0.05, 1)),
               "'p_thresholds' must have named elements")
  expect_error(ggcorrhm(mtcars, p_values = T, p_thresholds = c("***" = 0.001, "***" = 0.01, "*" = .05, 1)),
               "P-value threshold symbols must be unique")
  expect_warning(ggcorrhm(mtcars, p_values = F, cell_labels = T, cell_label_p = T),
                 "Writing correlation values as no p-values have been computed")
})

test_that("snapshots are ok", {
  vdiffr::expect_doppelganger("basic_ggcorrhm", ggcorrhm(mtcars))
  vdiffr::expect_doppelganger("asymmetric_corrhm", ggcorrhm(iris[1:32, -5], mtcars))
  vdiffr::expect_doppelganger("corr_layout", ggcorrhm(mtcars, layout = "tr"))
  vdiffr::expect_doppelganger("corr_w_options", ggcorrhm(mtcars, cluster_rows = T, cluster_cols = T,
                                                         annot_rows_df = data.frame(.names = colnames(mtcars), a = 1:ncol(mtcars)),
                                                         annot_cols_df = data.frame(.names = colnames(mtcars), b = 1:ncol(mtcars))))
  vdiffr::expect_doppelganger("cell_shape", ggcorrhm(mtcars, mode = 21))
  vdiffr::expect_doppelganger("p_values", ggcorrhm(mtcars, p_values = T, p_adjust = "fdr"))
  vdiffr::expect_doppelganger("mixed_layout", ggcorrhm(mtcars, layout = c("tl", "br")))
  vdiffr::expect_doppelganger("mixed_w_p", ggcorrhm(mtcars, layout = c("tr", "bl"), p_values = c(T, F)))
  vdiffr::expect_doppelganger("mixed_w_p_w_labels", ggcorrhm(mtcars, layout = c("tr", "bl"), p_values = c(FALSE, TRUE),
                                                             cell_labels = TRUE, cell_label_p = c(FALSE, TRUE),
                                                             p_adjust = "bonferroni"))
})
