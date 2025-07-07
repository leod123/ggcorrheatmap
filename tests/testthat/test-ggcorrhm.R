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

test_that("user-supplied scales", {
  # Change default scale using high, mid, low, etc
  expect_no_error(ggcorrhm(mtcars, high = "pink", mid = "white", low = "lightblue",
                           limits = c(-.7, .7), bins = 6))
  # Give a scale object
  expect_no_error(ggcorrhm(mtcars, fill_scale = ggplot2::scale_fill_distiller(palette = "RdBu")))
  expect_no_error(ggcorrhm(mtcars, mode = "19",
                           col_scale = ggplot2::scale_colour_distiller(palette = "RdBu")))
  # One scale for two triangles
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("hm", "hm"),
                           fill_scale = ggplot2::scale_fill_distiller(palette = "RdBu")))
  # Two scales
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("21", "23"),
                           fill_scale = list(
                             ggplot2::scale_fill_distiller(palette = "RdBu"),
                             ggplot2::scale_fill_continuous()
                           ),
                           size_scale = list(
                             ggplot2::scale_size_continuous(range = c(1, 6)),
                             ggplot2::scale_size_continuous(range = 4, 8)
                           )))
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("21", "23"),
                           fill_scale = list(
                             ggplot2::scale_fill_distiller(palette = "RdBu"),
                             NULL
                           ), high = "magenta", mid = "yellow", low = "green", bins = 6,
                           limits = c(-.5, .5)))
  # Also for colour scales
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("text", "text"),
                           col_scale = ggplot2::scale_colour_distiller(palette = "RdBu")))
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("19", "17"),
                           col_scale = list(
                             ggplot2::scale_colour_distiller(palette = "RdBu"),
                             NULL
                           ),
                           size_scale = list(
                             NULL,
                             ggplot2::scale_size_continuous()
                           ), bins = 4))
})

test_that("p-value errors work", {
  expect_error(ggcorrhm(mtcars, p_values = T, p_thresholds = c("a" = -1, "b" = 0.5, "c" = 1)),
               class = "p_thr_error")
  expect_error(ggcorrhm(mtcars, p_values = T, p_thresholds = c("***" = 0.001, "**" = 0.01, "*" = 0.05, .1)),
               class = "p_thr_error")
  expect_error(ggcorrhm(mtcars, p_values = T, p_thresholds = c(0.001, 0.01, 0.05, 1)),
               class = "p_thr_error")
  expect_error(ggcorrhm(mtcars, p_values = T, p_thresholds = c("***" = 0.001, "***" = 0.01, "*" = .05, 1)),
               class = "p_thr_error")
  expect_warning(ggcorrhm(mtcars, p_values = F, cell_labels = T, cell_label_p = T),
                 class = "cell_label_p_warn")
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
  lbl <- cor(mtcars)
  lbl[] <- 1:121
  vdiffr::expect_doppelganger("cell_labels", ggcorrhm(mtcars, cell_labels = lbl))
  vdiffr::expect_doppelganger("mixed_w_p_w_labels", ggcorrhm(mtcars, layout = c("tr", "bl"), p_values = c(FALSE, TRUE),
                                                             cell_labels = TRUE, cell_label_p = c(FALSE, TRUE),
                                                             p_adjust = "bonferroni"))
  vdiffr::expect_doppelganger("mixed_scales", ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("hm", "hm"),
                                                       fill_scale = list(
                                                         ggplot2::scale_fill_viridis_c(option = "G"),
                                                         ggplot2::scale_fill_gradient(high = "pink", low = "white")
                                                       ), fill_name = c("viridis", "gradient")))
  vdiffr::expect_doppelganger("mixed_scales2", ggcorrhm(mtcars, layout = c("tr", "bl"), mode = c("hm", "hm"),
                                                        fill_scale = list(
                                                          NULL,
                                                          ggplot2::scale_fill_gradient(high = "pink", low = "white")
                                                        ), fill_name = c("default", "gradient")))
  vdiffr::expect_doppelganger("mixed_scales3", ggcorrhm(mtcars, layout = c("tr", "bl"), mode = c("19", "17"),
                                                        col_scale = list(
                                                          ggplot2::scale_colour_viridis_c(option = "D"),
                                                          ggplot2::scale_colour_gradient(high = "pink", low = "white")
                                                        ), col_name = c("default", "gradient")))
})
