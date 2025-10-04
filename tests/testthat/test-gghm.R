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
  expect_no_error(gghm(mtcars, cluster_rows = TRUE, cluster_cols = TRUE,
                       annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:nrow(mtcars)),
                       annot_cols_df = data.frame(matrix(1:ncol(mtcars), ncol = 1,
                                                         dimnames = list(colnames(mtcars), "b"))),
                       dend_rows_extend = list(set = list("nodes_pch", 19),
                                               highlight_branches_col = list()),
                       dend_cols_extend = list(set = list("branches_k_color", k = 3),
                                               set = list("leaves_pch", 21)),
                       return_data = TRUE))
  # More things to cover
  expect_no_error(gghm(cor(mtcars), cluster_rows = TRUE, cluster_cols = TRUE, layout = "br",
                       mode = "21", legend_order = c(1, NA),
                       names_diag_params = list(angle = -45),
                       cell_labels = TRUE))
  expect_no_error(gghm(cor(mtcars), layout = c("tr", "bl"), mode = c("hm", "23"),
                       cluster_rows = TRUE, cluster_cols = TRUE))
  # Cell labels input types
  lbl1 <- lbl2 <- as.matrix(mtcars)
  lbl1[] <- lbl2[] <- NA
  lbl1[sample(1:length(lbl1), 100, FALSE)] <- 10
  lbl2[sample(1:length(lbl2), 100, FALSE)] <- "a"
  expect_no_error(gghm(mtcars, cell_labels = lbl1))
  expect_no_error(gghm(mtcars, cell_labels = lbl2))
  expect_no_error(gghm(cor(mtcars), layout = "br", split_diag = TRUE))
  expect_no_error(gghm(cor(mtcars), layout = c("br", "tl"), split_diag = TRUE,
                       col_scale = c("A", "G"), annot_rows_df = data.frame(
                         .names = colnames(mtcars), a = 1:11, b = 11:1
                       ), cluster_rows = TRUE, cluster_cols = TRUE))
  # Mixed layout with multiple scales
  expect_no_error(gghm(cor(mtcars), layout = c("tr", "bl"), mode = c("hm", "hm"),
                       col_scale = list(
                         ggplot2::scale_fill_gradient(high = "pink", low = "white",
                                                      guide = ggplot2::guide_colourbar(order = 1)),
                         "D"
                       ), annot_rows_df = data.frame(.names = colnames(mtcars),
                                                     a = 1:11, b = 11:1),
                       legend_order = c(NA, 4, 1, 3)))
  expect_no_error(gghm(cor(mtcars), layout = c("tl", "br"), mode = c("19", "19"),
                       col_scale = list(
                         ggplot2::scale_colour_gradient(high = "pink", low = "white"),
                         ggplot2::scale_colour_viridis_c()
                       ),
                       size_scale = list(
                         ggplot2::scale_size_continuous(range = c(1, 3)),
                         ggplot2::scale_size_continuous(range = c(5, 8))
                       ),
                       cluster_rows = TRUE, cluster_cols = TRUE))
  expect_equal(nrow(gghm(mtcars, return_data = TRUE)$plot_data), nrow(mtcars) * ncol(mtcars))
  expect_equal(nrow(gghm(cor(mtcars), return_data = TRUE)$plot_data), ncol(mtcars) * ncol(mtcars))
})

test_that("snapshots", {
  vdiffr::expect_doppelganger("basic_plot", gghm(mtcars))
  vdiffr::expect_doppelganger("w_options", gghm(mtcars, scale_data = "c", cluster_rows = TRUE, cluster_cols = TRUE,
                                                annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:nrow(mtcars)),
                                                annot_cols_df = data.frame(.names = colnames(mtcars), b = 1:ncol(mtcars))))
  vdiffr::expect_doppelganger("scaling_rows", gghm(mtcars, scale_data = "row"))
  vdiffr::expect_doppelganger("scaling_cols", gghm(mtcars, scale_data = "col", cluster_rows = TRUE, cluster_cols = TRUE))
  vdiffr::expect_doppelganger("cell_shape", gghm(mtcars, mode = "21"))
  vdiffr::expect_doppelganger("text_mode", gghm(mtcars, mode = "text"))
  vdiffr::expect_doppelganger("mixed_mode", gghm(cor(mtcars), layout = c("tl", "br")))
  vdiffr::expect_doppelganger("annotation labels", gghm(cor(mtcars),
                                                        annot_rows_df = data.frame(.names = colnames(mtcars), a = 1:11, b = 11:1),
                                                        annot_rows_names_side = "top",
                                                        annot_names_size = 3,
                                                        annot_rows_names_params = list(colour = "red", vjust = 0, angle = 45)) +
                                ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 45, vjust = 0)))
  vdiffr::expect_doppelganger("clustering_extended",
                              gghm(scale(mtcars), cluster_rows = TRUE, cluster_cols = TRUE,
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
                                   cluster_rows = TRUE, cluster_cols = TRUE,
                                   annot_dist = 2, annot_size = 1, annot_gap = 1,
                                   annot_cols_params = list(dist = .3, size = .5, gap = .1),
                                   dend_dist = 1, dend_cols_params = list(dist = 0.1)))
  vdiffr::expect_doppelganger("gaps", gghm(mtcars, split_rows = 16, split_cols = 5))
  vdiffr::expect_doppelganger("more_gaps", gghm(mtcars, split_rows = c(rep("A", 11), rep("B", 11), rep("C", 10)),
                                                split_cols = c(rep(1, 5), rep(2, 6))))
  vdiffr::expect_doppelganger("gaps_with_clustering", gghm(mtcars, scale_data = "col",
                                                           split_rows = 3, split_cols = 2,
                                                           cluster_rows = TRUE, cluster_cols = TRUE))
  vdiffr::expect_doppelganger("more_gaps_with_more_stuff",
                              gghm(volcano, split_rows = 45, split_cols = 30,
                                   annot_rows_df = data.frame(a = 1:87, b = 87:1),
                                   annot_cols_df = data.frame(c = 1:61, d = 61:1),
                                   border_col = 0))

  lbl1 <- as.matrix(mtcars)
  lbl1[] <- NA
  set.seed(123)
  lbl1[sample(1:length(lbl1), 100, FALSE)] <- 10
  vdiffr::expect_doppelganger("cell_labels_from_matrix", gghm(mtcars, cell_labels = lbl1))
  vdiffr::expect_doppelganger("mixed_scales1", gghm(cor(mtcars), layout = c("tr", "bl"), mode = c("hm", "hm"),
                                                    col_scale = list(
                                                      ggplot2::scale_fill_gradient(high = "pink", low = "white"),
                                                      "D"
                                                    )))
  vdiffr::expect_doppelganger("mixed_scale_params", {
    a <- cor(mtcars)
    a[c(2, 12, 14, 24, 26, 36)] <- NA
    gghm(a, layout = c("tl", "br"), mode = c("hm", "hm"),
         bins = c(4L, 6L), limits = list(c(-1, 1), c(-.5, .5)),
         col_scale = c("A", "D"), na_col = c("green", "blue"))
  })
  # Triangular layouts with asymmetric matrix
  set.seed(123)
  sq_df <- matrix(rnorm(100), nrow = 10, dimnames = list(letters[1:10], letters[1:10]))
  vdiffr::expect_doppelganger("asymm_square", gghm(sq_df, layout = "tl"))
  vdiffr::expect_doppelganger("asymm_sqare2", gghm(sq_df, layout = c("tl", "br"), mode = c("hm", "hm"), col_scale = c("A", "G")))
  vdiffr::expect_doppelganger("split_diag1", gghm(cor(mtcars), layout = "br", split_diag = TRUE))
  vdiffr::expect_doppelganger("split_diag2", gghm(cor(mtcars), layout = c("br", "tl"), mode = c("hm", "hm"),
                                                  col_scale = c("A", "G"),
                                                  split_diag = TRUE, cluster_rows = TRUE, cluster_cols = TRUE,
                                                  annot_rows_df = data.frame(
                                                    .names = colnames(mtcars), a = 1:11, b = 11:1
                                                  )))
  # Skip this test in CI
  # All scales are specified as ggplot2 scale objects, leaving the legend order completely to ggplot2
  # to automatically arrange. Seems like the order differs on different platforms, causing failure in CI
  skip_on_ci()
  vdiffr::expect_doppelganger("mixed_scales2", gghm(cor(mtcars), layout = c("tl", "br"), mode = c("19", "19"),
                                                    col_scale = list(
                                                      ggplot2::scale_colour_gradient(high = "pink", low = "white"),
                                                      ggplot2::scale_colour_viridis_c()
                                                    ),
                                                    size_scale = list(
                                                      ggplot2::scale_size_continuous(range = c(1, 3)),
                                                      ggplot2::scale_size_continuous(range = c(5, 8))
                                                    ),
                                                    cluster_rows = TRUE, cluster_cols = TRUE))
})

test_that("correct input types", {
  expect_error(gghm("a"), class = "input_class_error")
  expect_error(gghm(1), class = "input_class_error")
})

test_that("warnings and errors", {
  expect_error(gghm(cor(mtcars), mode = "nothing"), class = "nonsup_mode_error")
  expect_error(gghm(cor(mtcars), layout = c("tl", "br"), mode = c("hm", "heamtap")), class = "nonsup_mode_error")
  expect_error(gghm(cor(mtcars), layout = "nice"), class = "nonsup_layout_error")
  expect_warning(gghm(mtcars, layout = "br"), class = "force_full_warn")
  expect_error(gghm(mtcars, cluster_rows = "a"), class = "clust_class_error")
  expect_warning(gghm(mtcars, col_scale = 1), class = "scale_class_warn")
  expect_warning(gghm(mtcars, mode = "21", size_scale = "a"), class = "scale_class_warn")
  expect_warning(gghm(cor(mtcars), layout = "br", cluster_rows = TRUE), class = "force_clust_warn")
  expect_warning(gghm(cor(mtcars), layout = c("tl", "br"), cluster_rows = TRUE), class = "force_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_rows = TRUE), class = "unequal_clust_warn")
  expect_warning(gghm(cor(mtcars), cluster_cols = TRUE), class = "unequal_clust_warn")
  expect_warning(gghm(mtcars, cluster_rows = TRUE, dend_rows_extend = "A"), class = "extend_class_warn")
  expect_warning(gghm(mtcars, legend_order = "A"), class = "lgd_order_class_warn")
  expect_error(gghm(mtcars, cell_labels = NULL), class = "cell_labels_class_error")
  expect_warning(gghm(mtcars, cell_labels = iris), class = "cell_labels_rowcol_warn")
  # Diagonal names parameters
  expect_warning(gghm(cor(mtcars), show_names_diag = TRUE, names_diag_params = "a"),
                 class = "diag_names_arg_warn")
  # Input is clustering or dendrogram object
  # asymmetric matrix
  cl1 <- hclust(dist(mtcars))    # rows
  cl2 <- hclust(dist(t(mtcars))) # cols
  cl3 <- as.dendrogram(cl1)
  expect_no_error(gghm(mtcars, cluster_rows = cl1))
  expect_no_error(gghm(mtcars, cluster_rows = cl1, cluster_cols = cl2))
  expect_no_error(gghm(mtcars, cluster_rows = cl3, cluster_cols = cl2))
  expect_no_error(gghm(mtcars, cluster_rows = cl3, cluster_cols = TRUE))
  expect_error(gghm(mtcars, cluster_rows = cl2), class = "cluster_labels_error")
  expect_error(gghm(mtcars, cluster_cols = cl1), class = "cluster_labels_error")
  expect_error(gghm(mtcars, cluster_cols = cl3), class = "cluster_labels_error")

  # No warning even if non-identical clustering if the order is the same
  expect_no_warning(gghm(cor(mtcars), layout = "bl",
                         cluster_rows = hclust(dist(cor(mtcars))),
                         cluster_cols = hclust(dist(cor(mtcars), method = "manhattan"), method = "ward.D2")))
  # Warning for invalid colour scale when character input
  expect_warning(gghm(mtcars, col_scale = "ABC"), class = "invalid_colr_option_warn")
  expect_warning(gghm(mtcars, annot_cols_df = data.frame(.names = colnames(mtcars), a = 1:11, b = 11:1),
                      annot_cols_col = list(a = "A", b = "ASDF")), class = "invalid_colr_option_warn")
})

test_that("symmetric_matrix", {
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

})

test_that("clustering_and_layouts", {
  expect_no_warning(gghm(cor(mtcars), cluster_rows = TRUE, cluster_cols = TRUE))
  expect_no_warning(gghm(cor(mtcars), cluster_rows = TRUE, cluster_cols = TRUE,
                         dend_rows_side = "left", dend_cols_side = "top"))
  expect_no_warning(gghm(cor(mtcars), layout = "tl", cluster_rows = TRUE, cluster_cols = TRUE))
  expect_no_warning(gghm(cor(mtcars), layout = "tr", cluster_rows = TRUE, cluster_cols = TRUE))
  expect_no_warning(gghm(cor(mtcars), layout = "bl", cluster_rows = TRUE, cluster_cols = TRUE))
  expect_no_warning(gghm(cor(mtcars), layout = "br", cluster_rows = TRUE, cluster_cols = TRUE))
  expect_no_warning(gghm(cor(mtcars), layout = c("tl", "br"), cluster_rows = TRUE, cluster_cols = TRUE))
  expect_no_warning(gghm(cor(mtcars), layout = c("tr", "bl"), cluster_rows = TRUE, cluster_cols = TRUE))
  expect_no_warning(gghm(cor(mtcars), layout = c("tl", "br"), cluster_rows = TRUE, cluster_cols = TRUE,
                         dend_rows_side = "left", dend_cols_side = "top"))
  expect_no_warning(gghm(cor(mtcars), layout = c("tr", "bl"), cluster_rows = TRUE, cluster_cols = TRUE,
                         dend_rows_side = "left", dend_cols_side = "top"))
})

test_that("annotation_and_modes", {
  adf <- data.frame(.names = colnames(mtcars), a = 1:11, b = 11:1)
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf, mode = "none"))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf, mode = "text"))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf, mode = "21"))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf, mode = "10"))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf,
                       layout = c("tl", "br")))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf,
                       layout = c("tl", "br"), mode = c("hm", "none")))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf,
                       layout = c("tl", "br"), mode = c("none", "none")))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf,
                       layout = c("tl", "br"), mode = c("hm", "21")))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf,
                       layout = c("tl", "br"), mode = c("none", "21")))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf,
                       layout = c("tl", "br"), mode = c("10", "none")))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf,
                       layout = c("tl", "br"), mode = c("21", "10")))
  expect_no_error(gghm(cor(mtcars), annot_rows_df = adf, annot_cols_df = adf,
                       layout = c("tl", "br"), mode = c("10", "text")))
})

test_that("some_annotation_conditions", {
  # Invalid annotation colour scale option
  expect_warning(gghm(mtcars, annot_cols_df = data.frame(
    .names = c(colnames(mtcars)),
    a = 1:11, b = 11:1, c = sample(letters[1:3], 11, TRUE)
  ), annot_cols_col = list(a = "A", b = "A_rev", c = 3)),
  class = "annot_fill_class_warn")
  expect_no_warning(gghm(cor(mtcars), annot_rows_df = data.frame(
    .names = colnames(mtcars), a = 3:13, b = 4:14
  ), annot_rows_side = "left"))
  expect_warning(gghm(cor(mtcars), annot_rows_df = data.frame(
    .names = colnames(mtcars), a = 3:13, b = 4:14
  ), annot_rows_side = "asdf"),
  class = "annot_side_warn")
  expect_no_warning(gghm(cor(mtcars), annot_cols_df = data.frame(
    .names = colnames(mtcars), a = sample(letters[1:3], 11, TRUE)
  ), annot_rows_side = "something")) # specifying annot_rows_side for column annotation, no warning
  expect_warning(gghm(cor(mtcars), annot_cols_df = data.frame(
    .names = colnames(mtcars), a = sample(letters[1:3], 11, TRUE)
  ), annot_cols_side = "left"),
  class = "annot_side_warn")

  expect_warning(gghm(cor(mtcars), annot_rows_df = data.frame(.names = colnames(mtcars), a = 1:11, b = 11:1),
                      annot_rows_names_side = "asdf"), class = "annot_names_side_warn")
  # Annotation and dendrogram parameters
  expect_error(gghm(mtcars, annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:32, b = 32:1),
                    annot_dist = "a"), class = "annot_nonnum_error")
  ## No error if overwritten with parameter list
  expect_no_error(gghm(mtcars, annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:32, b = 32:1),
                       annot_dist = "a", annot_rows_params = list(dist = 1)))
  ## But still error if only one is overwritten when both should be drawn
  expect_error(gghm(mtcars, annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:32, b = 32:1),
                    annot_cols_df = data.frame(.names = colnames(mtcars), c = 1:11),
                    annot_dist = "a", annot_rows_params = list(dist = 1)),
               class = "annot_nonnum_error")
})

test_that("some_dendrogram_conditions", {
  expect_no_warning(gghm(mtcars, cluster_rows = TRUE, dend_rows_side = "left"))
  expect_no_warning(gghm(mtcars, cluster_rows = TRUE, dend_cols_side = "left"))
  expect_warning(gghm(mtcars, cluster_rows = TRUE, dend_rows_side = "top"), class = "dend_side_warn")
  expect_warning(gghm(mtcars, cluster_cols = TRUE, dend_cols_side = "asdf"), class = "dend_side_warn")
  expect_error(gghm(mtcars, cluster_rows = TRUE, cluster_cols = TRUE, dend_height = "a"),
               class = "dend_nonnum_error")
  expect_no_error(gghm(mtcars, cluster_rows = TRUE, cluster_cols = FALSE, dend_height = "a",
                       dend_rows_params = list(height = 1)))
  expect_error(gghm(mtcars, cluster_rows = TRUE, cluster_cols = TRUE, dend_height = "a",
                    dend_rows_params = list(height = 1)),
               class = "dend_nonnum_error")
  expect_error(gghm(mtcars, annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:32, b = 32:1),
                    annot_rows_names_params = 1),
               class = "annot_name_params_class_error")
  # Annotation and dendrogram parameters, input class
  expect_warning(gghm(cor(mtcars), annot_rows_df = data.frame(.names = colnames(mtcars), a = 1:11, b = 5:15),
                      annot_rows_params = "a"),
                 class = "annot_params_warn")
  expect_warning(gghm(mtcars, cluster_rows = TRUE, dend_rows_params = 1),
                 class = "dend_params_warn")
  ## Names of elements
  expect_warning(gghm(cor(mtcars), annot_rows_df = data.frame(.names = colnames(mtcars), a = 1:11, b = 5:15),
                      annot_rows_params = list(a = "a", col = 1)),
                 class = "replace_default_warn")
  expect_warning(gghm(mtcars, cluster_rows = TRUE, dend_rows_params = list(height = 1, asdf = 2)),
                 class = "replace_default_warn")
})

test_that("other_logical_arguments", {
  expect_error(gghm(mtcars, cluster_rows = TRUE, show_dend_rows = "TRUE!!"), class = "logical_error")
  expect_error(gghm(mtcars, cluster_rows = TRUE, cluster_cols = TRUE, show_dend_cols = 1), class = "logical_error")
  expect_error(gghm(mtcars, na_remove = "A"), class = "logical_error")
  expect_error(gghm(mtcars, annot_cols_df = data.frame(.names = colnames(mtcars), a = 1:11),
                    annot_na_remove = "asdf"), class = "logical_error")
  expect_error(gghm(mtcars, return_data = "ASDF"), class = "logical_error")
  expect_error(gghm(cor(mtcars), show_names_diag = "TRUE"), class = "logical_error")
  expect_error(gghm(cor(mtcars), show_names_cols = "TR"), class = "logical_error")
  expect_error(gghm(cor(mtcars), show_names_rows = "TRU"), class = "logical_error")
  expect_error(gghm(cor(mtcars), include_diag = "TRUE"), class = "logical_error")
  expect_error(gghm(cor(mtcars), annot_rows_df = data.frame(.names = colnames(mtcars), a = 1:11),
                    show_annot_names = "false"), class = "logical_error")
})

test_that("other_class_checks", {
  expect_error(gghm(mtcars, annot_cols_df = data.frame(.names = colnames(mtcars), a = c(NA, 1:10)),
                    annot_na_col = NULL), class = "annot_na_col_length_error")
  expect_error(gghm(mtcars, limits = 1), class = "numeric_error")
  expect_error(gghm(mtcars, limits = "a"), class = "numeric_error")
  expect_error(gghm(iris[1:20, -5], bins = TRUE), class = "numeric_error")
  expect_error(gghm(iris[1:20, -5], bins = c(1, 2)), class = "numeric_error")
  # Too few bins in when float numbers
  # (if 2 there is an error, if 1 or lower it keeps running without finising)
  expect_error(gghm(mtcars, bins = 1), class = "float_bin_error")
  # coercion to double vector
  expect_error(gghm(cor(mtcars), layout = c("tl", "br"), bins = c(2, 4L)), class = "float_bin_error")
  # also in a list
  expect_error(gghm(cor(mtcars), layout = c("tl", "br"), bins = list(2, 4L)), class = "float_bin_error")
  # Scaling of data
  expect_error(gghm(mtcars, scale_data = "asdf"), class = "scale_data_error")
})

test_that("facet_conditions", {
  expect_warning(gghm(mtcars, split_rows = 16, split_rows_side = "a"), class = "facet_side_warn")
  expect_warning(gghm(mtcars, split_cols = 5, split_cols_side = 1), class = "facet_side_warn")
  expect_no_warning(gghm(mtcars, split_rows = 16, split_cols_side = "asdf"))
  expect_error(gghm(mtcars, cluster_rows = TRUE, split_rows = c(10, 20)), class = "facet_clust_error")
  expect_warning(gghm(mtcars, split_cols = c("mpg" = 3, "disp" = 5)), class = "facet_names_warn")
  fc <- rep(1:3, 4)[1:11] # make vector where names are the colnames, but one has been changed
  names(fc) <- colnames(mtcars)
  names(fc)[1] <- "a"
  expect_warning(gghm(mtcars, split_cols = fc), class = "facet_names_warn")
  expect_error(gghm(mtcars, split_rows = "asdf"), class = "invalid_facet_input_error")
  expect_error(gghm(mtcars, split_rows = data.frame()), class = "invalid_facet_input_error")
  expect_no_error(gghm(mtcars, split_cols = c(1, 4, 2)))
  expect_error(gghm(mtcars, split_cols = c(1, 2, 2)), class = "facet_ind_error")
  expect_error(gghm(mtcars, split_cols = c(1, 1.4, 1.6, 1.9)), class = "facet_ind_error")
  expect_error(gghm(mtcars, split_rows = c(-2, -1, 0)), class = "facet_ind_error")
})

test_that("annotation names must exist in the data", {
  expect_no_error(gghm(mtcars, annot_rows_df =
                         data.frame(.names = rownames(mtcars),
                                    annot1 = rnorm(nrow(mtcars)),
                                    annot2 = sample(letters[1:3], nrow(mtcars), TRUE))))
  expect_warning(gghm(mtcars, annot_rows_df =
                        data.frame(.names = c(rownames(mtcars), "asdf", "qwer"),
                                   annot1 = rnorm(nrow(mtcars) + 2),
                                   annot2 = sample(letters[1:3], nrow(mtcars) + 2, TRUE))),
                 class = "annot_names_warn")
  expect_warning(gghm(mtcars, annot_cols_df =
                        data.frame(.names = c(colnames(mtcars), "asdf", "qwer"),
                                   annot1 = rnorm(ncol(mtcars) + 2),
                                   annot2 = sample(letters[1:3], ncol(mtcars) + 2, TRUE))),
                 class = "annot_names_warn")
  # Check for duplicated names
  expect_error(gghm(mtcars, annot_cols_df = data.frame(
    .names = c(colnames(mtcars), "mpg", "mpg", "hp", "carb"),
    a = 1:15, b = 15:1, c = sample(letters[1:3], 15, TRUE)
  )), class = "dupl_annot_name_error")
})

test_that("mixed_layout_errors", {
  expect_warning(gghm(mtcars, layout = c("tl", "br")), class = "force_full_warn")
  expect_error(gghm(cor(mtcars), layout = c("tr", "br")), class = "nonsup_layout_error")
  expect_error(gghm(cor(mtcars), layout = c("tl", "br"), mode = "heatmap"), class = "layout_mode_len_error")
  expect_error(gghm(cor(mtcars), layout = c("too", "many", "layouts", "!")), class = "layout_mode_len_error")
  expect_error(gghm(cor(mtcars), layout = c("tl", "br"), border_col = c()), class = "param_len_error")
})

test_that("deprecated_annot_name_params", {
  expect_no_warning(gghm(mtcars,
                         annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:32, b = 32:1),
                         annot_cols_df = data.frame(.names = colnames(mtcars), c = 1:11, d = 11:1),
                         annot_rows_names_params = list(colour = "red"),
                         annot_cols_names_params = list(angle = 300)))
  expect_warning(gghm(mtcars,
                      annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:32, b = 32:1),
                      annot_cols_df = data.frame(.names = colnames(mtcars), c = 1:11, d = 11:1),
                      annot_rows_name_params = list(rot = 100)),
                 class = "annot_names_depr")
  expect_warning(gghm(mtcars,
                      annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:32, b = 32:1),
                      annot_cols_df = data.frame(.names = colnames(mtcars), c = 1:11, d = 11:1),
                      annot_cols_name_params = list(rot = 50)),
                 class = "annot_names_depr")
  expect_warning(gghm(mtcars,
                      annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:32, b = 32:1),
                      annot_cols_df = data.frame(.names = colnames(mtcars), c = 1:11, d = 11:1),
                      annot_rows_name_params = list(rot = 100),
                      annot_rows_names_params = list(colour = "green")),
                 class = "annot_names_depr_both")
  expect_warning(gghm(mtcars,
                      annot_rows_df = data.frame(.names = rownames(mtcars), a = 1:32, b = 32:1),
                      annot_cols_df = data.frame(.names = colnames(mtcars), c = 1:11, d = 11:1),
                      annot_cols_name_params = list(rot = 100),
                      annot_cols_names_params = list(colour = "green")),
                 class = "annot_names_depr_both")
})
