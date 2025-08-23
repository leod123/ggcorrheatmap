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
  expect_error(gghm_tidy(), class = "tidy_missing_error")
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
  dat2 <- data.frame(row = rep(1:32, 4),
                     col = rep(colnames(iris)[-5], each = 32),
                     val = unlist(iris[1:32, -5]),
                     extra = 1)
  # cor_long
  expect_error(cor_long(), class = "tidy_missing_error")
  expect_error(cor_long(dat), class = "tidy_missing_error")
  expect_error(cor_long(dat, rows), class = "tidy_missing_error")
  expect_error(cor_long(dat, rows, cols), class = "tidy_missing_error")
  expect_error(cor_long(dat, c(rows, extra), cols, vals), class = "tidy_too_many_cols_error")
  expect_error(cor_long(dat, rows, cols, vals, dat2), class = "tidy_too_few_args_error")
  expect_error(cor_long(dat, rows, cols, vals, dat2, row), class = "tidy_too_few_args_error")
  expect_error(cor_long(dat, rows, cols, vals, dat2, row, col), class = "tidy_too_few_args_error")
  expect_error(cor_long(dat, rows, cols, vals, dat2, col, val), class = "tidy_too_few_args_error")
  expect_error(cor_long(dat, rows, cols, vals, dat2, row, col, c(val, extra)), class = "tidy_too_many_cols_error")
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

  # Facets
  facet_df <- data.frame(nm = colnames(mtcars), fc = c(rep("a", 4), rep("b", 3), rep("c", 4)))
  dat4 <- dplyr::left_join(dat3, dplyr::rename(facet_df, frow = fc), by = c("rows" = "nm"))
  dat4 <- dplyr::left_join(dat4, dplyr::rename(facet_df, fcol = fc), by = c("cols" = "nm"))
  expect_error(gghm_tidy(dat4, rows, cols, vals, facet_rows = frow, facet_cols = fcol,
                         split_rows = 5, split_cols = 5), class = "tidy_split_error")
  expect_error(gghm_tidy(dat4, rows, cols, vals, facet_rows = c(frow, fcol)), class = "tidy_facet_too_many")
})

test_that("snapshots", {
  dat <- data.frame(rows = rep(rownames(mtcars), ncol(mtcars)),
                    cols = rep(colnames(mtcars), each = nrow(mtcars)),
                    vals = unlist(mtcars))
  row_annot <- data.frame(rows = rownames(mtcars), a = 1:32, rf = c(rep("A", 11), rep("B", 11), rep("C", 10)))
  col_annot <- data.frame(cols = colnames(mtcars), b = letters[c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2)], cf = c(rep("asdf", 5), rep("qwer", 6)))
  dat$row_annot <- row_annot[match(dat$rows, row_annot$rows), "a", drop = TRUE]
  dat$col_annot <- col_annot[match(dat$cols, col_annot$cols), "b", drop = TRUE]
  dat$row_facet <- row_annot[match(dat$rows, row_annot$rows), "rf", drop = TRUE]
  dat$col_facet <- col_annot[match(dat$cols, col_annot$cols), "cf", drop = TRUE]

  vdiffr::expect_doppelganger("gghm_tidy", gghm_tidy(dat, rows, cols, vals))
  vdiffr::expect_doppelganger("gghm_tidy_w_options", gghm_tidy(dat, rows, cols, vals,
                                                               col_scale = "A", cluster_rows = TRUE, cluster_cols = TRUE))
  vdiffr::expect_doppelganger("gghm_tidy_w_extra_all", gghm_tidy(dat, rows, cols, vals,
                                                                 labels = vals,
                                                                 annot_rows = row_annot,
                                                                 annot_cols = col_annot,
                                                                 cluster_rows = TRUE, cluster_cols = TRUE,
                                                                 col_scale = "Blues",
                                                                 scale_data = "col",
                                                                 cell_label_digits = 0))
  vdiffr::expect_doppelganger("gghm_tidy_w_facets", gghm_tidy(dat, rows, cols, vals,
                                                              facet_rows = row_facet,
                                                              facet_cols = col_facet))
  vdiffr::expect_doppelganger("gghm_tidy_facets_n_more", gghm_tidy(dat, rows, cols, vals,
                                                                   facet_rows = row_facet,
                                                                   facet_cols = col_facet,
                                                                   annot_rows = row_annot,
                                                                   annot_cols = col_annot))
  vdiffr::expect_doppelganger("gghm_tidy_everything", gghm_tidy(dat, rows, cols, vals,
                                                                annot_rows = row_annot,
                                                                annot_cols = col_annot,
                                                                scale_data = "col",
                                                                cluster_rows = TRUE,
                                                                cluster_cols = TRUE,
                                                                # Use the split_* arguments to work with clustering
                                                                split_rows = 3,
                                                                split_cols = 2))

  dat$rows2 <- factor(dat$rows, levels = rownames(mtcars)[32:1])
  dat$cols2 <- factor(dat$cols, levels = colnames(mtcars)[11:1])
  vdiffr::expect_doppelganger("gghm_tidy_factors", gghm_tidy(dat, rows2, cols2, vals))

  vdiffr::expect_doppelganger("ggcorrhm_tidy", ggcorrhm_tidy(dat, rows, cols, vals, cor_in = FALSE))
  vdiffr::expect_doppelganger("ggcorrhm_tidy_w_more", ggcorrhm_tidy(dat, rows, cols, vals, cor_in = FALSE,
                                                                    labels = TRUE, annot_rows = col_annot,
                                                                    annot_cols = col_annot, cluster_rows = TRUE,
                                                                    cluster_cols = TRUE, col_scale = c("A", "G"),
                                                                    layout = c("tr", "bl"), mode = c("hm", "hm")))
  cor_dat <- cor_long(dat, rows, cols, vals, out_format = "long")
  cor_annot <- data.frame(nm = colnames(mtcars),
                          row_annot = LETTERS[c(rep(1, 5), rep(2, 4), rep(3, 2))],
                          col_annot = 1:11)
  cor_dat$row_annot <- cor_annot[match(cor_dat$row, cor_annot$nm), "row_annot", drop = TRUE]
  cor_dat$col_annot <- cor_annot[match(cor_dat$col, cor_annot$nm), "col_annot", drop = TRUE]

  vdiffr::expect_doppelganger("ggcorrhm_tidy_cor_in", ggcorrhm_tidy(cor_dat, row, col, value))
  vdiffr::expect_doppelganger("ggcorrhm_tidy_cor_in_more",
                              ggcorrhm_tidy(cor_dat, row, col, value, labels = value,
                                            annot_rows = row_annot, annot_cols = col_annot,
                                            cluster_rows = TRUE, cluster_cols = TRUE,
                                            layout = c("tl", "br"), high = c("pink", "red"),
                                            mid = "white", low = c("lightblue", "blue"),
                                            mode = c("hm", "hm")))
  cor_dat2 <- cor_long(dat, rows, cols, vals,
                       shape_mat_long(iris[1:32, -5]), row, col, value, out_format = "long")
  vdiffr::expect_doppelganger("ggcorrhm_tidy_asymmetric",
                              ggcorrhm_tidy(cor_dat2, row, col, value))

  dat$lab <- 1:nrow(dat)
  vdiffr::expect_doppelganger("gghm_labels", gghm_tidy(dat, rows, cols, vals, labels = lab, cell_label_col = "white"))
  vdiffr::expect_doppelganger("gghm_labels_scale", gghm_tidy(dat, rows, cols, vals, scale_data = "col",
                                                             labels = vals, cell_label_col = "white"))
})
