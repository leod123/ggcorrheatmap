test_that("basic functionality works", {
  expect_no_error(gghm(mtcars))
  # Even without row or colnames
  expect_no_error(gghm(replicate(10, rnorm(15))))
  # Different dimensions
  expect_no_error(gghm(mtcars[1:2, ]))
  expect_no_error(gghm(mtcars[, 1:2]))
  expect_no_error(gghm(mtcars[1:2, 1:2]))
  # Clustering and annotation
  expect_no_error(gghm(mtcars, cluster_rows = T, cluster_cols = T,
                       annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:nrow(mtcars)),
                       annot_cols_df = data.frame(matrix(1:ncol(mtcars), ncol = 1,
                                                         dimnames = list(colnames(mtcars), "b"))),
                       dend_rows_extend = list(set = list("nodes_pch", 19),
                                               highlight_branches_col = list()),
                       dend_cols_extend = list(set = list("branches_k_color", k = 3),
                                               set = list("leaves_pch", 21)),
                       return_data = T))
  # More things to cover
  expect_no_error(gghm(cor(mtcars), cluster_rows = T, cluster_cols = T, layout = "br",
                       legend_position = c(0.2, 0.8), cell_shape = 21,
                       show_legend = c("fill" = T, "size" = F),
                       names_diag_param = list(angle = -45),
                       label_cor = T))
})

test_that("snapshots", {
  vdiffr::expect_doppelganger("basic_plot", gghm(mtcars))
  vdiffr::expect_doppelganger("w_options", gghm(scale(mtcars), cluster_rows = T, cluster_cols = T,
                                                annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:nrow(mtcars)),
                                                annot_cols_df = data.frame(.names = colnames(mtcars), b = 1:ncol(mtcars))))
  vdiffr::expect_doppelganger("cell_shape", gghm(mtcars, cell_shape = 21))
})

test_that("correct input types", {
  expect_error(gghm("a"), "x must be a matrix or data frame")
  expect_error(gghm(1), "x must be a m")
})

test_that("warnings for layouts and clustering", {
  expect_warning(gghm(mtcars, layout = "br"), "A triangular layout with an a")
  expect_warning(gghm(cor(mtcars), layout = "br", cluster_rows = T), "Cannot cluster only one")
  expect_warning(gghm(cor(mtcars), layout = "bl",
                      cluster_rows = hclust(dist(cor(mtcars))),
                      cluster_cols = hclust(dist(cor(mtcars), method = "manhattan"), method = "ward.D2")),
                 "If the row and column clusterings are not identical")
})

test_that("annotation names must exist in the data", {
  expect_no_error(gghm(mtcars, annot_rows_df =
                         data.frame(.names = rownames(mtcars),
                                    annot1 = rnorm(nrow(mtcars)),
                                    annot2 = sample(letters[1:3], nrow(mtcars), T))))
  expect_error(gghm(mtcars, annot_rows_df =
                      data.frame(.names = c(rownames(mtcars), "asdf", "qwer"),
                                 annot1 = rnorm(nrow(mtcars) + 2),
                                 annot2 = sample(letters[1:3], nrow(mtcars) + 2, T))),
               "Some names in the row annotation")
  expect_error(gghm(mtcars, annot_cols_df =
                      data.frame(.names = c(colnames(mtcars), "asdf", "qwer"),
                                 annot1 = rnorm(ncol(mtcars) + 2),
                                 annot2 = sample(letters[1:3], ncol(mtcars) + 2, T))),
               "Some names in the column annotation")
  # Row annotations are checked first
  expect_error(gghm(mtcars,
                    annot_rows_df =
                      data.frame(.names = c(rownames(mtcars), "asdf", "qwer"),
                                 annot1 = rnorm(nrow(mtcars) + 2),
                                 annot2 = sample(letters[1:3], nrow(mtcars) + 2, T)),
                    annot_cols_df =
                      data.frame(.names = c(colnames(mtcars), "asdf", "qwer"),
                                 annot3 = rnorm(ncol(mtcars) + 2),
                                 annot4 = sample(letters[1:3], ncol(mtcars) + 2, T))),
               "Some names in the row annotation")
})
