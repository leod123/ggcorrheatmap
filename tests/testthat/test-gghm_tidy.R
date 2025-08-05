### ADD TESTS FOR THE NEW ERRORS AND WARNINGS!!!

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

test_that("gghm_errors", {
  dat <- data.frame(rows = rep(rownames(mtcars), ncol(mtcars)),
                    cols = rep(colnames(mtcars), each = nrow(mtcars)),
                    vals = unlist(mtcars),
                    extra = seq(1, nrow(mtcars) * ncol(mtcars)))
  expect_error(gghm_tidy(dat), class = "tidy_missing_error")
  expect_error(gghm_tidy(dat, rows), class = "tidy_missing_error")
  expect_error(gghm_tidy(dat, rows, cols), class = "tidy_missing_error")
  expect_error(gghm_tidy(dat, rows, cols, c(vals, extra)), class = "tidy_too_many_cols_error")
  expect_error(gghm_tidy(1, 2, 3, 4), class = "tidy_input_class_error")
})

test_that("ggcorrhm_errors", {
  dat <- data.frame(rows = rep(rownames(mtcars), ncol(mtcars)),
                    cols = rep(colnames(mtcars), each = nrow(mtcars)),
                    vals = unlist(mtcars),
                    extra = seq(1, nrow(mtcars) * ncol(mtcars)))
  dcor <- cor_long(dat, rows, cols, vals)
  expect_error(ggcorrhm_tidy(dat, rows, cols, cor_in = FALSE), class = "tidy_missing_error")
  expect_error(ggcorrhm_tidy(dcor, rows, cor_in = TRUE), class = "tidy_missing_error")
  expect_error(ggcorrhm_tidy(dcor, cor_in = TRUE), class = "tidy_missing_error")
  expect_error(ggcorrhm_tidy(), class = "tidy_missing_error")
  expect_error(ggcorrhm_tidy(1, rows, cols, vals), class = "tidy_input_class_error")
  expect_error(ggcorrhm_tidy(dat, rows, c(cols, extra), vals, cor_in = FALSE), class = "tidy_too_many_cols_error")
})

test_that("other_errors", {
  dat <- data.frame(rows = rep(rownames(mtcars), ncol(mtcars)),
                    cols = rep(colnames(mtcars), each = nrow(mtcars)),
                    vals = unlist(mtcars),
                    extra = seq(1, nrow(mtcars) * ncol(mtcars)))
  dat2 <- data.frame(rows = rep(1:32, 4),
                     cols = rep(colnames(iris)[-5], each = 32),
                     vals = unlist(iris[1:32, -5]),
                     extra = 1)
  # cor_long
  expect_error(cor_long(), class = "tidy_missing_error")
  expect_error(cor_long(dat), class = "tidy_missing_error")
  expect_error(cor_long(dat, rows), class = "tidy_missing_error")
  expect_error(cor_long(dat, rows, cols), class = "tidy_missing_error")
  expect_error(cor_long(dat, c(rows, extra), cols, vals), class = "tidy_too_many_cols_error")
  expect_error(cor_long(dat, rows, cols, vals, dat2, rows), class = "tidy_too_few_args_error")
  expect_error(cor_long(dat, rows, cols, vals, dat2, rows, cols), class = "tidy_too_few_args_error")
  expect_error(cor_long(dat, rows, cols, vals, dat2, col, vals), class = "tidy_too_few_args_error")
  expect_error(cor_long(dat, rows, cols, vals, dat2, rows, cols, c(vals, extra)), class = "tidy_too_many_cols_error")
  expect_error(cor_long(dat, rows, cols, vals, p_values = "a"), class = "logical_error")
  expect_error(cor_long(dat, rows, cols, vals, p_values = TRUE, p_sym_digits = "asdf"),
               class = "numeric_error")
  expect_error(cor_long(dat, rows, cols, vals, p_values = TRUE, p_thresholds = letters[1:5]),
               class = "p_thr_class_error")
  expect_error(cor_long(dat, rows, cols, vals, p_values = TRUE,
                        p_thresholds = c("a" = 0.1, "b" = NA, "c" = 0.3, "d" = 1)),
               class = "p_thr_error")
  expect_error(cor_long(dat, rows, cols, vals, p_values = TRUE,
                        p_thresholds = c("a" = -0.1, "b" = 0.2, "c" = 0.3, "d" = 1)),
               class = "p_thr_error")
  expect_error(cor_long(dat, rows, cols, vals, p_values = TRUE,
                        p_thresholds = c("a" = 0.1, "b" = 0.2, "c" = 0.3, "d" = 0.4)),
               class = "p_thr_error")
  expect_error(cor_long(dat, rows, cols, vals, p_values = TRUE,
                        p_thresholds = c(0.1, 0.2, 0.3, 1)),
               class = "p_thr_error")
  expect_error(cor_long(dat, rows, cols, vals, p_values = TRUE,
                        p_thresholds = c("a" = 0.1, "b" = 0.2, 0.3, 1)),
               class = "p_thr_error")
  expect_warning(cor_long(dat, rows, cols, vals, p_values = TRUE, p_sym_add = "asdf"),
                 class = "p_sym_option_warn")
  # add_mixed_layout
  dat3 <- data.frame(rows = rep(colnames(mtcars), ncol(mtcars)),
                     cols = rep(colnames(mtcars), each = ncol(mtcars)),
                     vals = as.vector(cor(mtcars)),
                     extra = 1:121)
  expect_error(add_mixed_layout(dat3, c(rows, extra), cols, vals, layout = c("tl", "br")),
               class = "tidy_too_many_cols_error")
  expect_error(add_mixed_layout(dat, rows, cols, vals, layout = c("tr", "bl")), class = "nonsym_error")
  expect_error(add_mixed_layout(dat3, rows, cols, vals, layout = "w"), class = "nonsup_layout_error")
  expect_warning(add_mixed_layout(dat3, rows, cols, vals, layout = c("tl", "br"), name = "cols"),
                 class = "name_exists_warn")
})

test_that("snapshots", {
  dat <- data.frame(rows = rep(rownames(mtcars), ncol(mtcars)),
                    cols = rep(colnames(mtcars), each = nrow(mtcars)),
                    vals = unlist(mtcars))
  vdiffr::expect_doppelganger("gghm_tidy", gghm_tidy(dat, rows, cols, vals))
  vdiffr::expect_doppelganger("gghm_tidy_w_options", gghm_tidy(dat, rows, cols, vals,
                                                               col_scale = "A", cluster_rows = TRUE, cluster_cols = TRUE))

  dat$rows2 <- factor(dat$rows, levels = rownames(mtcars)[32:1])
  dat$cols2 <- factor(dat$cols, levels = colnames(mtcars)[11:1])
  vdiffr::expect_doppelganger("gghm_tidy_factors", gghm_tidy(dat, rows2, cols2, vals))

  vdiffr::expect_doppelganger("ggcorrhm_tidy", ggcorrhm_tidy(dat, rows, cols, vals, cor_in = FALSE))
  cor_dat <- cor_long(dat, rows, cols, vals, out_format = "long")
  vdiffr::expect_doppelganger("ggcorrhm_tidy_cor_in", ggcorrhm_tidy(cor_dat, row, col, value))

  dat$lab <- 1:nrow(dat)
  vdiffr::expect_doppelganger("gghm_labels", gghm_tidy(dat, rows, cols, vals, labels = lab, cell_label_col = "white"))
  vdiffr::expect_doppelganger("gghm_labels_scale", gghm_tidy(dat, rows, cols, vals, scale_data = "col",
                                                             labels = vals, cell_label_col = "white"))
})
