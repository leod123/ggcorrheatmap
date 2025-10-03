test_that("it runs without error", {
  expect_no_error(ggcorrhm(mtcars))
  expect_no_error(ggcorrhm(mtcars, bins = 5, mode = "21"))
  # Two input matrices
  expect_no_error(ggcorrhm(mtcars, iris[1:32, -5], return_data = TRUE, cell_labels = TRUE))
  # P-values
  expect_no_error(ggcorrhm(mtcars, p_values = TRUE, p_adjust = "fdr", return_data = TRUE))
  expect_no_error(ggcorrhm(mtcars, p_values = TRUE, p_adjust = "fdr", return_data = FALSE, cell_labels = TRUE))
  expect_no_error(ggcorrhm(mtcars, iris[1:32, -5], p_values = TRUE))
  expect_no_error(ggcorrhm(iris[1:32, -5], mtcars, p_values = TRUE, p_adjust = "bonferroni"))
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("hm", "18")))
  expect_no_error(ggcorrhm(mtcars, annot_rows_df = data.frame(.names = colnames(mtcars), a = 1:11, b = 11:1),
                           layout = c("tl", "br"), mode = c("hm", "hm")))
  # Number of rows in returned data
  expect_equal(nrow(ggcorrhm(mtcars, return_data = TRUE)$plot_data), ncol(mtcars) * ncol(mtcars))
  expect_equal(nrow(ggcorrhm(mtcars, return_data = TRUE, layout = c("tl", "br"))$plot_data),
               ncol(mtcars) * ncol(mtcars))
})

test_that("class errors", {
  expect_error(ggcorrhm(mtcars, midpoint = c(1, 2, 3)), class = "numeric_error")
  expect_error(ggcorrhm(mtcars, midpoint = NULL), class = "numeric_error")
  expect_error(ggcorrhm(mtcars, midpoint = "A"), class = "numeric_error")
  expect_error(ggcorrhm(mtcars, mode = "21", size_range = "ASDF"), class = "numeric_error")
  expect_error(ggcorrhm(mtcars, size_range = "ASDF"), class = "numeric_error")
  expect_error(ggcorrhm(mtcars, mode = "21", size_range = c(1, 2, 3)), class = "numeric_error")
  expect_error(ggcorrhm(mtcars, p_values = "TRUE"), class = "logical_error")
  expect_error(ggcorrhm(mtcars, p_values = c(TRUE, FALSE)), class = "logical_error")
  expect_error(ggcorrhm(mtcars, p_values = list(TRUE, c(TRUE, FALSE)), layout = c("tl", "br")),
               class = "logical_error")
  # Error on cell_label_p even if no p-values are computed
  expect_error(ggcorrhm(mtcars, cell_labels = TRUE, cell_label_p = list(TRUE)), class = "logical_error")
  expect_error(ggcorrhm(mtcars, cell_labels = TRUE, cell_label_p = c(TRUE, FALSE)), class = "logical_error")
  expect_error(ggcorrhm(mtcars, cell_labels = TRUE, cell_label_p = c(TRUE, FALSE),
                        p_values = TRUE), class = "logical_error")
  expect_error(ggcorrhm(mtcars, cell_labels = TRUE, cell_label_p = list(c(TRUE, TRUE), FALSE),
                        layout = c("tr", "bl"), mode = c("hm", "hm")),
               class = "logical_error")
})

test_that("user-supplied scales", {
  # Change default scale using high, mid, low, etc
  expect_no_error(ggcorrhm(mtcars, high = "pink", mid = "white", low = "lightblue",
                           limits = c(-.7, .7), bins = 6))
  # Also for colour scales
  expect_no_error(ggcorrhm(mtcars, high = "pink", mid = "white", low = "lightblue",
                           limits = c(-.7, .7), bins = 6, mode = "text"))
  # Use a Brewer or Viridis scale
  expect_no_error(ggcorrhm(mtcars, col_scale = "G"))
  expect_no_error(ggcorrhm(mtcars, mode = "19", col_scale = "RdYlGn", bins = 5))
  expect_no_error(ggcorrhm(mtcars, high = "pink", mid = "white", low = "lightblue",
                           col_scale = "RdBu"))
  # Give a scale object
  expect_no_error(ggcorrhm(mtcars, col_scale = ggplot2::scale_fill_distiller(palette = "RdBu")))
  # One scale for two triangles
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("hm", "hm"),
                           col_scale = "Purples"))
  # Two scales
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("21", "23"),
                           col_scale = list(
                             "RdBu",
                             "Blues"
                           ),
                           size_scale = list(
                             ggplot2::scale_size_continuous(range = c(1, 6)),
                             ggplot2::scale_size_continuous(range = c(4, 8))
                           )))
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("21", "23"),
                           col_scale = list(
                             ggplot2::scale_fill_distiller(palette = "RdBu"),
                             ggplot2::scale_fill_continuous()
                           ),
                           size_scale = list(
                             ggplot2::scale_size_continuous(range = c(1, 6)),
                             ggplot2::scale_size_continuous(range = c(4, 8))
                           )))
  expect_no_error(ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("19", "23"),
                           col_scale = list(
                             ggplot2::scale_colour_distiller(palette = "RdBu"),
                             NULL
                           ), high = "magenta", mid = "yellow", low = "green", bins = 6,
                           limits = c(-.5, .5)))
})

test_that("p-value errors work", {
  expect_no_error(ggcorrhm(mtcars, p_values = TRUE, p_thresholds = NULL))
  expect_error(ggcorrhm(mtcars, p_values = TRUE, p_thresholds = c("a", "b", "c")),
               class = "p_thr_class_error")
  expect_error(ggcorrhm(mtcars, p_values = TRUE, p_thresholds = c("a" = -1, "b" = 0.5, "c" = 1)),
               class = "p_thr_error")
  expect_error(ggcorrhm(mtcars, p_values = TRUE, p_thresholds = c("a" = 0.01, "b" = 0.05, "c" = NA, "d" = 1)),
               class = "p_thr_error")
  expect_error(ggcorrhm(mtcars, p_values = TRUE, p_thresholds = c("***" = 0.001, "**" = 0.01, "*" = 0.05, .1)),
               class = "p_thr_error")
  expect_error(ggcorrhm(mtcars, p_values = TRUE, p_thresholds = c(0.001, 0.01, 0.05, 1)),
               class = "p_thr_error")
  expect_error(ggcorrhm(mtcars, p_values = TRUE, p_thresholds = c("***" = 0.001, "***" = 0.01, "*" = .05, 1)),
               class = "p_thr_error")
  expect_warning(ggcorrhm(mtcars, p_values = FALSE, cell_labels = TRUE, cell_label_p = TRUE),
                 class = "cell_label_p_warn")
  expect_error(ggcorrhm(mtcars, p_values = TRUE, p_adjust = "asdf"), class = "p_adjust_error")
})

test_that("snapshots are ok", {
  vdiffr::expect_doppelganger("basic_ggcorrhm", ggcorrhm(mtcars))
  vdiffr::expect_doppelganger("asymmetric_corrhm", ggcorrhm(iris[1:32, -5], mtcars))
  # Check that diagonal names end up in the correct positions regardless of inclusion of diagonal
  vdiffr::expect_doppelganger("diag_names1", ggcorrhm(mtcars, layout = "tr", show_names_rows = TRUE, show_names_c = TRUE))
  vdiffr::expect_doppelganger("diag_names2", ggcorrhm(mtcars, layout = "tr", include_diag = FALSE, show_names_cols = TRUE, show_names_rows = TRUE))
  vdiffr::expect_doppelganger("diag_names3", ggcorrhm(mtcars, layout = "tr", include_diag = FALSE, show_names_diag = FALSE, show_names_cols = TRUE, show_names_rows = TRUE))
  vdiffr::expect_doppelganger("diag_names4", ggcorrhm(mtcars, layout = "tr", include_diag = TRUE, show_names_diag = FALSE, show_names_cols = TRUE, show_names_rows = TRUE))
  vdiffr::expect_doppelganger("corr_w_options", ggcorrhm(mtcars, cluster_rows = TRUE, cluster_cols = TRUE,
                                                         annot_rows_df = data.frame(.names = colnames(mtcars), a = 1:ncol(mtcars)),
                                                         annot_cols_df = data.frame(.names = colnames(mtcars), b = 1:ncol(mtcars))))
  vdiffr::expect_doppelganger("cell_shape", ggcorrhm(mtcars, mode = 21))
  vdiffr::expect_doppelganger("p_values", ggcorrhm(mtcars, p_values = TRUE, p_adjust = "fdr"))
  vdiffr::expect_doppelganger("mixed_layout", ggcorrhm(mtcars, layout = c("tl", "br")))
  vdiffr::expect_doppelganger("mixed_w_p", ggcorrhm(mtcars, layout = c("tr", "bl"), p_values = c(TRUE, FALSE)))
  lbl <- cor(mtcars)
  lbl[] <- 1:121
  vdiffr::expect_doppelganger("cell_labels", ggcorrhm(mtcars, cell_labels = lbl))
  vdiffr::expect_doppelganger("mixed_w_p_w_labels", ggcorrhm(mtcars, layout = c("tr", "bl"), p_values = c(FALSE, TRUE),
                                                             cell_labels = TRUE, cell_label_p = c(FALSE, TRUE),
                                                             p_adjust = "bonferroni"))
  vdiffr::expect_doppelganger("mixed_scales", ggcorrhm(mtcars, layout = c("tl", "br"), mode = c("hm", "hm"),
                                                       col_scale = list(
                                                         "G",
                                                         "RdPu"
                                                       ), col_name = c("viridis", "gradient")))
  vdiffr::expect_doppelganger("mixed_scales2", ggcorrhm(mtcars, layout = c("tr", "bl"), mode = c("hm", "hm"),
                                                        col_scale = list(
                                                          NULL,
                                                          ggplot2::scale_fill_gradient(high = "pink", low = "white")
                                                        ),
                                                        # Second name is ignored since it's a scale in col_scale
                                                        col_name = c("default", "gradient")))
  vdiffr::expect_doppelganger("mixed_scales3", ggcorrhm(mtcars, layout = c("tr", "bl"), mode = c("19", "17"),
                                                        col_scale = list(
                                                          ggplot2::scale_colour_viridis_c(option = "D"),
                                                          ggplot2::scale_colour_gradient(high = "pink", low = "white")
                                                        ),
                                                        # Both names are ignored
                                                        col_name = c("default", "gradient")))
  vdiffr::expect_doppelganger("mixed_scale_param1", {
    a <- cor(mtcars)
    a[c(2, 12, 14, 24, 26, 36)] <- NA
    ggcorrhm(a, cor_in = TRUE, layout = c("tr", "bl"), mode = c("hm", "hm"),
             high = c("pink", "green"),
             mid = c("white", "yellow"),
             low = c("lightblue", "red"),
             limits = list(c(-1, 1), c(-.75, .75)),
             bins = c(4L, 5L),
             na_col = c("beige", "magenta"))
  })
  vdiffr::expect_doppelganger("mixed_scale_param2", {
    ggcorrhm(mtcars, layout = c("tr", "bl"), mode = c("21", "18"),
             high = c("pink", "green"),
             mid = c("white", "yellow"),
             low = c("lightblue", "red"),
             limits = list(c(-1, 1), c(-.75, .75)),
             bins = c(4L, 5L),
             na_col = c("beige", "magenta"),
             size_range = list(c(6), c(7, 14)),
             legend_order = 1:10)
  })
  vdiffr::expect_doppelganger("facets", ggcorrhm(mtcars, split_rows = 5, split_cols = 5))
  vdiffr::expect_doppelganger("facets_layout1", ggcorrhm(mtcars, split_rows = 5, split_cols = 5, layout = "bl"))
  vdiffr::expect_doppelganger("facets_layout2", ggcorrhm(mtcars, split_rows = 5, split_cols = 5, layout = c("tl", "br")))
})
