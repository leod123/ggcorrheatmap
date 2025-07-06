test_that("basic functionality works", {
  expect_no_error(gghm(mtcars))
  # Even without row or colnames
  expect_no_error(gghm(replicate(10, rnorm(15))))
  # Different dimensions
  expect_no_error(gghm(mtcars[1:2, ]))
  expect_no_error(gghm(mtcars[, 1:2]))
  expect_no_error(gghm(mtcars[1:2, 1:2]))
  # A heatmap with discrete values (with coercion)
  expect_no_error(gghm(iris[1:5, ]))
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
                       mode = "21", show_legend = c("fill" = T, "size" = F),
                       names_diag_param = list(angle = -45),
                       cell_labels = T))
  expect_no_error(gghm(cor(mtcars), layout = c("tr", "bl"), mode = c("hm", "23"),
                       cluster_rows = T, cluster_cols = T))
  # Cell labels input types
  lbl1 <- lbl2 <- as.matrix(mtcars)
  lbl1[] <- lbl2[] <- NA
  lbl1[sample(1:length(lbl1), 100, F)] <- 10
  lbl2[sample(1:length(lbl2), 100, F)] <- "a"
  expect_no_error(gghm(mtcars, cell_labels = lbl1))
  expect_no_error(gghm(mtcars, cell_labels = lbl2))
  # Mixed layout with multiple scales
  expect_no_error(gghm(cor(mtcars), layout = c("tr", "bl"), mode = c("hm", "hm"),
                       fill_scale = list(
                         ggplot2::scale_fill_gradient(high = "pink", low = "white"),
                         ggplot2::scale_fill_viridis_c()
                       )))
  expect_no_error(gghm(cor(mtcars), layout = c("tl", "br"), mode = c("19", "19"),
                       col_scale = list(
                         ggplot2::scale_colour_gradient(high = "pink", low = "white"),
                         ggplot2::scale_colour_viridis_c()
                       ),
                       size_scale = list(
                         ggplot2::scale_size_continuous(range = c(1, 3)),
                         ggplot2::scale_size_continuous(range = c(5, 8))
                       ),
                       cluster_rows = T, cluster_cols = T))
})

test_that("snapshots", {
  vdiffr::expect_doppelganger("basic_plot", gghm(mtcars))
  vdiffr::expect_doppelganger("w_options", gghm(scale(mtcars), cluster_rows = T, cluster_cols = T,
                                                annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:nrow(mtcars)),
                                                annot_cols_df = data.frame(.names = colnames(mtcars), b = 1:ncol(mtcars))))
  vdiffr::expect_doppelganger("cell_shape", gghm(mtcars, mode = "21"))
  vdiffr::expect_doppelganger("text_mode", gghm(mtcars, mode = "text"))
  vdiffr::expect_doppelganger("mixed_mode", gghm(cor(mtcars), layout = c("tl", "br")))
  vdiffr::expect_doppelganger("clustering_extended",
                              gghm(scale(mtcars), cluster_rows = T, cluster_cols = T,
                                   dend_height = 1,
                                   dend_rows_extend = list(
                                     set = list("by_labels_branches_col", value = rownames(mtcars)[1:5], TF_values = "red"),
                                     set = list("by_labels_branches_lty", value = rownames(mtcars)[10:12], TF_values = 3),
                                     set = list("by_labels_branches_lwd", value = rownames(mtcars)[21:25], TF_values = 1),
                                     set = list("nodes_pch", 19),
                                     set = list("nodes_cex", 2),
                                     set = list("nodes_col", "orange"),
                                     set = list("leaves_pch", 21),
                                     set = list("leaves_cex", 3),
                                     set = list("leaves_col", "purple"),
                                     raise.dendrogram = list(1)
                                   ),
                                   dend_cols_extend = list(
                                     highlight_branches_col = NULL,
                                     set = list("nodes_pch", c(15, 16, 17)),
                                     set = list("nodes_cex", 2:4),
                                     set = list("nodes_col", as.character(1:5))
                                   )))
  vdiffr::expect_doppelganger("annotation_cluster_distances",
                              gghm(scale(mtcars[1:10, ]),
                                   annot_rows_df = data.frame(.names = rownames(mtcars)[1:10],
                                                              a = 1:10, b = 10:1, c = 1:10),
                                   annot_cols_df = data.frame(.names = colnames(mtcars),
                                                              a = 1:11, b = 11:1, c = 1:11),
                                   cluster_rows = T, cluster_cols = T,
                                   annot_dist = 2, annot_size = 1, annot_gap = 1,
                                   annot_cols_params = list(dist = .3, size = .5, gap = .1),
                                   dend_dist = 1, dend_cols_params = list(dist = 0.1)))
  lbl1 <- as.matrix(mtcars)
  lbl1[] <- NA
  set.seed(123)
  lbl1[sample(1:length(lbl1), 100, F)] <- 10
  vdiffr::expect_doppelganger("cell_labels_from_matrix", gghm(mtcars, cell_labels = lbl1))
  vdiffr::expect_doppelganger("mixed_scales1", gghm(cor(mtcars), layout = c("tr", "bl"), mode = c("hm", "hm"),
                                                    fill_scale = list(
                                                      ggplot2::scale_fill_gradient(high = "pink", low = "white"),
                                                      ggplot2::scale_fill_viridis_c()
                                                    )))
  vdiffr::expect_doppelganger("mixed_scales2", gghm(cor(mtcars), layout = c("tl", "br"), mode = c("19", "19"),
                                                    col_scale = list(
                                                      ggplot2::scale_colour_gradient(high = "pink", low = "white"),
                                                      ggplot2::scale_colour_viridis_c()
                                                    ),
                                                    size_scale = list(
                                                      ggplot2::scale_size_continuous(range = c(1, 3)),
                                                      ggplot2::scale_size_continuous(range = c(5, 8))
                                                    ),
                                                    cluster_rows = T, cluster_cols = T))
})

test_that("correct input types", {
  expect_error(gghm("a"), class = "input_class_error")
  expect_error(gghm(1), class = "input_class_error")
})

test_that("warnings for layouts and clustering", {
  expect_error(gghm(cor(mtcars), mode = "nothing"), class = "nonsup_mode_error")
  expect_error(gghm(cor(mtcars), layout = "nice"), class = "nonsup_layout_error")
  expect_warning(gghm(mtcars, layout = "br"), class = "force_full_warn")
  expect_warning(gghm(cor(mtcars), layout = "br", cluster_rows = T), class = "force_clust_warn")
  expect_warning(gghm(cor(mtcars), layout = c("tl", "br"), cluster_rows = T), class = "force_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_rows = T), class = "unequal_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_cols = T), class = "unequal_clust_warn")
  # Input is clustering or dendrogram object
  # asymmetric matrix
  cl1 <- hclust(dist(mtcars))    # rows
  cl2 <- hclust(dist(t(mtcars))) # cols
  cl3 <- as.dendrogram(cl1)
  expect_no_error(gghm(mtcars, cluster_rows = cl1))
  expect_no_error(gghm(mtcars, cluster_rows = cl1, cluster_cols = cl2))
  expect_no_error(gghm(mtcars, cluster_rows = cl3, cluster_cols = cl2))
  expect_no_error(gghm(mtcars, cluster_rows = cl3, cluster_cols = T))
  expect_error(gghm(mtcars, cluster_rows = cl2), class = "cluster_labels_error")
  expect_error(gghm(mtcars, cluster_cols = cl1), class = "cluster_labels_error")
  expect_error(gghm(mtcars, cluster_cols = cl3), class = "cluster_labels_error")

  # symmetric matrix
  cl1 <- hclust(dist(cor(mtcars)))
  cl2 <- as.dendrogram(cl1)
  cl3 <- dendextend::rotate(cl2, 11:1)

  expect_no_warning(gghm(cor(mtcars), cluster_rows = cl1, cluster_cols = cl2))
  expect_warning(gghm(cor(mtcars), cluster_rows = cl1), class = "unequal_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_rows = cl2), class = "unequal_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_rows = cl3), class = "unequal_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_rows = cl3, cluster_cols = cl2), class = "unequal_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_rows = cl3, layout = "tl"), class = "force_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_rows = cl3, layout = c("tl", "br")), class = "force_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_rows = cl3, cluster_cols = cl1, layout = c("tl", "br")), class = "unequal_clust_warn")

  # No warnings for different layouts with clustering
  expect_no_warning(gghm(cor(mtcars), cluster_rows = T, cluster_cols = T))
  expect_no_warning(gghm(cor(mtcars), cluster_rows = T, cluster_cols = T,
                         dend_rows_side = "left", dend_cols_side = "top"))
  expect_no_warning(gghm(cor(mtcars), layout = "tl", cluster_rows = T, cluster_cols = T))
  expect_no_warning(gghm(cor(mtcars), layout = "tr", cluster_rows = T, cluster_cols = T))
  expect_no_warning(gghm(cor(mtcars), layout = "bl", cluster_rows = T, cluster_cols = T))
  expect_no_warning(gghm(cor(mtcars), layout = "br", cluster_rows = T, cluster_cols = T))
  expect_no_warning(gghm(cor(mtcars), layout = c("tl", "br"), cluster_rows = T, cluster_cols = T))
  expect_no_warning(gghm(cor(mtcars), layout = c("tr", "bl"), cluster_rows = T, cluster_cols = T))
  expect_no_warning(gghm(cor(mtcars), layout = c("tl", "br"), cluster_rows = T, cluster_cols = T,
                         dend_rows_side = "left", dend_cols_side = "top"))
  expect_no_warning(gghm(cor(mtcars), layout = c("tr", "bl"), cluster_rows = T, cluster_cols = T,
                         dend_rows_side = "left", dend_cols_side = "top"))
  # No warning even if non-identical clustering if the order is the same
  expect_no_warning(gghm(cor(mtcars), layout = "bl",
                         cluster_rows = hclust(dist(cor(mtcars))),
                         cluster_cols = hclust(dist(cor(mtcars), method = "manhattan"), method = "ward.D2")))
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
               class = "annot_names_error")
  expect_error(gghm(mtcars, annot_cols_df =
                      data.frame(.names = c(colnames(mtcars), "asdf", "qwer"),
                                 annot1 = rnorm(ncol(mtcars) + 2),
                                 annot2 = sample(letters[1:3], ncol(mtcars) + 2, T))),
               class = "annot_names_error")
})

test_that("mixed_layout_errors", {
  expect_warning(gghm(mtcars, layout = c("tl", "br")), "Triangular layouts are not supported for asymmetric matrices")
  expect_error(gghm(cor(mtcars), layout = c("tr", "br")), class = "nonsup_layout_error")
  expect_error(gghm(cor(mtcars), layout = c("tl", "br"), mode = "heatmap"), class = "layout_mode_len_error")
  expect_error(gghm(cor(mtcars), layout = c("too", "many", "layouts", "!")), class = "layout_mode_len_error")
  expect_error(gghm(cor(mtcars), layout = c("tl", "br"), border_col = c()), class = "param_len_error")
})
