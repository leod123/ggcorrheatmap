test_that("it runs", {
  dat <- data.frame(rows = rep(rownames(mtcars), ncol(mtcars)),
                    cols = rep(colnames(mtcars), each = nrow(mtcars)),
                    vals = unlist(mtcars))
  expect_no_error(gghm_tidy(dat, rows, cols, vals))
  expect_no_error(ggcorrhm_tidy(dat, rows, cols, vals))
  expect_no_error(cor_long(dat, rows, cols, vals))

  cor_dat <- cor_long(dat, rows, cols, vals, out_format = "long")
  expect_no_error(ggcorrhm_tidy(cor_dat, row, col, value, cor_in = FALSE))
})

test_that("errors", {
  dat <- data.frame(rows = rep(rownames(mtcars), ncol(mtcars)),
                    cols = rep(colnames(mtcars), each = nrow(mtcars)),
                    vals = unlist(mtcars),
                    extra = seq(1, nrow(mtcars) * ncol(mtcars)))
  expect_error(gghm_tidy(dat, rows, cols), class = "tidy_missing_error")
  expect_error(gghm_tidy(dat, rows, cols, c(vals, extra)), class = "tidy_too_many_cols_error")
  expect_error(gghm_tidy(1, 2, 3, 4), class = "tidy_input_class_error")
})

test_that("snapshots", {
  dat <- data.frame(rows = rep(rownames(mtcars), ncol(mtcars)),
                    cols = rep(colnames(mtcars), each = nrow(mtcars)),
                    vals = unlist(mtcars))
  vdiffr::expect_doppelganger("gghm_tidy", gghm_tidy(dat, rows, cols, vals))
  vdiffr::expect_doppelganger("gghm_tidy_w_options", gghm_tidy(dat, rows, cols, vals,
                                                               col_scale = "A", cluster_rows = TRUE, cluster_cols = TRUE))
  vdiffr::expect_doppelganger("ggcorrhm_tidy", ggcorrhm_tidy(dat, rows, cols, vals, cor_in = FALSE))
  cor_dat <- cor_long(dat, rows, cols, vals, out_format = "long")
  vdiffr::expect_doppelganger("ggcorrhm_tidy_cor_in", ggcorrhm_tidy(cor_dat, row, col, value))
  dat$lab <- 1:nrow(dat)
  vdiffr::expect_doppelganger("gghm_labels", gghm_tidy(dat, rows, cols, vals, labels = lab, cell_label_col = "white"))
  vdiffr::expect_doppelganger("gghm_labels_scale", gghm_tidy(dat, rows, cols, vals, scale_data = "col",
                                                             labels = vals, cell_label_col = "white"))
})
