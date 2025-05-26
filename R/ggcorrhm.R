#' Make a correlation heatmap with ggplot2.
#'
#' Wrapper function for `gghm` to make a correlation heatmap from input matrices. Uses a diverging colour scale centered around 0.
#'
#' @param x Matrix or data frame in wide format containing the columns to correlate against each other or against the columns in `y`.
#' @param y Optional matrix or data frame in wide format containing columns to correlate with the columns in `x`.
#' @param cor_method String specifying correlation method to use in the `cor` function. Default is 'pearson'.
#' @param cor_use String specifying the `use` argument of `cor`, which defineshow to deal with missing values. Default is 'everything'.
#' @param high Name of the colour to use for the highest value of the colour scale.
#' @param mid Name of the colour to use for 0 in the colour scale.
#' @param low Name of the colour to use for the lowest value of the colour scale.
#' @param limits Correlation limits to plot between.
#' @param bins Specify number of bins if the correlation scale should be binned. NULL for a continuous scale.
#' @param fill_name String to use for the correlation colour scale. If NULL (default) the text will depend on the correlation method.
#' @param na_remove Logical indicating if NA values in the heatmap should be omitted (meaning no cell border is drawn). This does not affect how
#' NAs are handled in the correlation computations, use the `cor_use` argument for NA handling in correlation.
#' @param na_col Colour to use cells with NA.
#' @param layout String specifying the layout of the output correlation heatmap. Possible layouts include
#' top left, top right, bottom left, bottom right, or the whole heatmap (default and only possible option if the correlation matrix is asymmetric).
#' The string should be composed of the vertical position (top or bottom) followed by the horizontal position (left or right).
#' Bottom can be specified by 'bottom', 'lower', 'down', or the first letter of these. Left is specified by 'left' or 'l'.
#' 'full', 'whole', or 'all' (or 'f', 'w', 'a') result in the whole correlation matrix being plotted.
#' For any other strings top and right are selected.
#' @param include_diag Logical indicating if the diagonal cells should be plotted (included either way if the whole matrix is plotted).
#' @param return_data Logical indicating if the data used for plotting (i.e. the correlation values) should be returned.
#' @param cell_shape Value specifying what shape the heatmap cells should take. Any non-numeric value will result in a normal heatmap with square cells (default).
#' A numeric value can be used to specify an R shape (pch) to use, such as 21 for filled circles. Note that only shapes 21-25 support filling (others will not display the heatmap colour properly).
#' @param label_cells Logical specifying if the cells should be labelled with the correlation values.
#' @param cell_label_size Size of cell labels, used as the `size` argument in `ggplot2::geom_text`.
#' @param cell_label_digits Number of digits to display when cells are labelled with correlation coefficients. Default is 2, passed to `round`.
#' @param border_col Colour of cell borders.
#' @param border_lwd Size of cell borders, used for the `size` argument in `ggplot2::geom_tile`. Set to 0 to remove cell borders.
#' @param names_diag Logical indicating if names should be written in the diagonal cells.
#' @param names_diag_param List with named parameters (such as size, angle, etc) passed on to geom_text when writing the column names in the diagonal.
#' @param names_x Logical indicating if names should be written on the x axis. Labels can be customised using `ggplot2::theme()` on the output plot.
#' @param names_x_side String specifying position of the x axis names ("top" or "bottom").
#' @param names_y Logical indicating if names should be written on the y axis.
#' @param names_y_side String specifying position of the y axis names ("left" or "right").
#' @param annot_rows_df Data frame for row annotations. The names of the columns in the data must be included,
#' either as row names or in a column named `.names`. Each other column specifies an annotation where the column name
#' will be used as the annotation name (in the legend and next to the annotation). Numeric columns will use a continuous
#' colour scale while factor or character columns use discrete scales.
#' @param annot_cols_df Same usage as `annot_rows_df` but for column annotation.
#' @param annot_rows_fill Named list for row annotation colour scales. The names should specify which annotation the scales apply to.
#' Elements can be strings or ggplot2 "Scale" class objects. If a string it is used as the brewer palette (categorical annotation) or viridis option (continuous annotation).
#' If a scale object it is used as is, allowing more flexibility. This may change the order that legends are drawn in,
#' specify order using the `guide` argument in the `ggplot2` scale function provided.
#' @param annot_cols_fill Named list used for column annotation colour scales, used like `annot_rows_fill`.
#' @param annot_rows_side String specifying which side row annotation should be drawn ('left' for left, otherwise right).
#' @param annot_cols_side String specifying which side column annotation should be drawn ('bottom', 'down', 'lower' for bottom, otherwise top).
#' @param annot_legend Logical indicating if row and column annotations should have legends.
#' @param annot_dist Distance between heatmap and first annotation cell where 1 is the size of one heatmap cell. Used for both row and column annotation.
#' @param annot_gap Distance between each annotation where 1 is the size of one heatmap cell. Used for both row and column annotation.
#' @param annot_size Size (width for row annotation, height for column annotation) of annotation cells. Used for both row and column annotation.
#' @param annot_label not yet implemented (control if names of annotations should be shown in drawing area)
#' @param annot_rows_params Named list with parameters for row annotations to overwrite the defaults set by the `annot_*` arguments, each name corresponding to the `*` part
#' (see details for more information).
#' @param annot_cols_params Named list with parameters for column annotations, used like `annot_rows_params`.
#' @param annot_rows_label_side String specifying which side the row annotation labels should be on. Either "top" or "bottom".
#' @param annot_cols_label_side String specifying which side the column annotation labels should be on. Either "left" or "right".
#' @param annot_rows_label_params Named list of parameters for row annotation labels. Given to `grid::textGrob`, see `?grid::textGrob` for details. `?grid::gpar` is also helpful.
#' @param annot_cols_label_params Named list of parameters for column annotation labels. Given to `grid::textGrob`, see `?grid::textGrob` for details. `?grid::gpar` is also helpful.
#' @param cluster_rows Logical indicating if rows should be clustered.
#' @param cluster_cols Logical indicating if columns should be clustered.
#' @param cluster_distance String with the distance metric to use for clustering, given to `dist`.
#' @param cluster_method String with the clustering method to use, given to `hclust`.
#' @param dend_rows Logical indicating if a dendrogram should be drawn for the rows.
#' @param dend_cols Logical indicating if a dendrogram should be drawn for the columns.
#' @param dend_rows_side Which side to draw the row dendrogram on ('left' for left, otherwise right).
#' @param dend_cols_side Which side to draw the column dendrogram on ('bottom', 'down', 'lower' for bottom, otherwise top).
#' @param dend_col Colour to use for dendrogram lines, applied to both row and column dendrograms.
#' @param dend_height Number by which to scale dendrogram height, applied to both row and column dendrograms.
#' @param dend_lwd Linewidth of dendrogram lines, applied to both row and column dendrograms.
#' @param dend_lty Dendrogram line type, applied to both row and column dendrograms.
#' @param dend_rows_params Named list for row dendrogram parameters. See details for more information.
#' @param dend_cols_params Named list for column dendrogram parameters. See details for more information.
#' @param dend_rows_extend Named list for specifying `dendextend` functions to apply to the row dendrogram. See details for usage.
#' @param dend_cols_extend Named list for specifying `dendextend` functions to apply to the column dendrogram. See details for usage.
#' @param legend_position Position of the legends, given to `ggplot2::theme`. Either a string for the position outside the plotting area,
#' or a numeric vector of length 2 for normalised coordinates inside the plotting area.
#' @param plot_margin Plot margins, specified as a numeric vector of length 4 in the order of top, right, bottom, left.
#' @param margin_unit Unit to use for the specified margin.
#'
#' @return The correlation heatmap as a `ggplot` object.
#' If `return_data` is TRUE the output is a list containing the plot (named 'plot'),
#' the correlations ('plot_data'), and the result of the clustering ('clustering', only if `cluster_data` is TRUE).
#' @export
#'
#' @details
#' The annotation parameter arguments `annot_rows_params` and `annot_cols_params` should be named lists, where the possible options correspond to
#' the different `annot_*` arguments. The possible options are "legend" (logical, if legends should be drawn), "dist" (distance between heatmap and annotation), "gap" (distance between annotations),
#' "size" (cell size), "label" (logical, if the annotation names should be displayed), and "label_size" (label text size). Any unused options will
#' use the defaults set by the `annot_*` arguments.
#'
#' The dendrogram parameters arguments `dend_rows_params` and `dend_cols_params` should be named lists, analogous to the annotation parameter arguments. Possible options are
#' "col" (line colour), "height" (height scaling), "lwd" (line width), and "lty" (line type).
#'
#' The `dend_rows_extend` and `dend_cols_extend` arguments make it possible to customise the dendrograms using the `dendextend` package.
#' The argument should be a named list, each element named after the `dendextend` function to use (consecutive usage of the `set` function
#' is supported due to duplicate list names being possible). Each element should contain any arguments given to the `dendextend` function,
#' such as the `what` argument used in the `set` function. See examples for example usage.
#'
#' @examples
#' # Basic usage
#' ggcorrhm(mtcars)
#'
#' # With two matrices
#' ggcorrhm(iris[1:32, -5], mtcars)
#'
#' # Different layout
#' ggcorrhm(mtcars, layout = "br")
#'
#' # With clustering
#' ggcorrhm(mtcars, layout = "tl", cluster_rows = T, cluster_cols = T)
#'
#' # With annotation
#' set.seed(123)
#' annot <- data.frame(.names = colnames(mtcars),
#'                     annot1 = rnorm(ncol(mtcars)),
#'                     annot2 = sample(letters[1:3], ncol(mtcars), T))
#' ggcorrhm(mtcars, layout = "tr", annot_rows_df = annot,
#'          # Change margins to fit annotation labels
#'          plot_margin = c(20, 10, 60, 20))
#'
#' # Using the dend_options argument
#' ggcorrhm(mtcars, cluster_rows = T, dend_rows_extend =
#'   list("set" = list("branches_lty", c(1, 2, 3)),
#'        "set" = list("branches_k_color", k = 3),
#'        # Empty list element (or NULL) if no arguments to be given
#'        "highlight_branches_lwd" = list()))
ggcorrhm <- function(x, y = NULL, cor_method = "pearson", cor_use = "everything",
                     high = "sienna2", mid = "white", low = "skyblue2",
                     limits = c(-1, 1), bins = NULL, fill_name = NULL, #...
                     na_remove = F, na_col = "grey",
                     layout = "full", include_diag = F, return_data = F,
                     cell_shape = "heatmap", label_cells = F, cell_label_size = 3, cell_label_digits = 2,
                     border_col = "grey", border_lwd = 0.5,
                     names_diag = T, names_diag_param = NULL,
                     names_x = F, names_x_side = "top", names_y = F, names_y_side = "left",
                     annot_rows_df = NULL, annot_cols_df = NULL, annot_rows_fill = NULL, annot_cols_fill = NULL,
                     annot_rows_side = "right", annot_cols_side = "bottom",
                     annot_legend = T, annot_dist = 0.2, annot_gap = 0, annot_size = 0.5, annot_label = T,
                     annot_rows_params = NULL, annot_cols_params = NULL,
                     annot_rows_label_side = "bottom", annot_cols_label_side = "left",
                     annot_rows_label_params = NULL, annot_cols_label_params = NULL,
                     cluster_rows = F, cluster_cols = F,
                     cluster_distance = "euclidean", cluster_method = "complete",
                     dend_rows = T, dend_cols = T, dend_rows_side = "right", dend_cols_side = "bottom",
                     dend_col = "black", dend_height = 0.3, dend_lwd = 0.3, dend_lty = 1,
                     dend_rows_params = NULL, dend_cols_params = NULL,
                     dend_rows_extend = NULL, dend_cols_extend = NULL,
                     legend_position = "right",
                     plot_margin = c(20, 10, 10, 20), margin_unit = "pt") {

  cor_mat <- if (is.null(y)) {
    cor(x, method = cor_method, use = cor_use)
  } else {
    cor(x, y, method = cor_method, use = cor_use)
  }

  # Name of fill legend
  if (is.null(fill_name)) {
    fill_name <- switch(cor_method,
                        "pearson" = "Pearson r",
                        "spearman" = "Spearman\nrho",
                        "kendall" = "Kendall\ntau")
  }

  # Don't display names on the diagonal if the plot is non-symmetric as it will cause
  # new ghost columns to be added to draw the names where row == col
  if (!isSymmetric(cor_mat)) {
    names_diag <- F
    # Also display x and y names by default, but remove if specified as FALSE (when specified as a named argument)
    names_x <- eval(replace_default(list("names_x" = T), as.list(sys.call()))$names_x)
    names_y <- eval(replace_default(list("names_y" = T), as.list(sys.call()))$names_y)
  }

  # Make the fill colour scale
  fill_scale <- if (!is.null(bins)) {
    ggplot2::scale_fill_steps2(limits = limits, high = high, mid = mid, low = low, na.value = na_col,
                               breaks = seq(limits[1], limits[2], length.out = bins), guide = ggplot2::guide_colourbar(order = 1))
  } else {
    ggplot2::scale_fill_gradient2(limits = limits, high = high, mid = mid, low = low, na.value = na_col,
                                  guide = ggplot2::guide_colourbar(order = 1))
  }

  # Call with all arguments to get the tooltips when calling this wrapper function
  cor_plt <- gghm(cor_mat, fill_scale = fill_scale, fill_name = fill_name, na_remove = na_remove,
                  layout = layout, include_diag = include_diag, return_data = return_data,
                  cell_shape = cell_shape, label_cells = label_cells, cell_label_size = cell_label_size,
                  cell_label_digits = cell_label_digits, border_col = border_col, border_lwd = border_lwd,
                  names_diag = names_diag, names_diag_param = names_diag_param,
                  names_x = names_x, names_x_side = names_x_side,
                  names_y = names_y, names_y_side = names_y_side,
                  annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df,
                  annot_rows_fill = annot_rows_fill, annot_cols_fill = annot_cols_fill,
                  annot_rows_side = annot_rows_side, annot_cols_side = annot_cols_side,
                  annot_legend = annot_legend, annot_dist = annot_dist, annot_gap = annot_gap,
                  annot_size = annot_size, annot_label = annot_label,
                  annot_rows_params = annot_rows_params, annot_cols_params = annot_cols_params,
                  annot_rows_label_side = annot_rows_label_side, annot_cols_label_side = annot_cols_label_side,
                  annot_rows_label_params = annot_rows_label_params, annot_cols_label_params = annot_cols_label_params,
                  cluster_rows = cluster_rows, cluster_cols = cluster_cols,
                  cluster_distance = cluster_distance, cluster_method = cluster_method,
                  dend_rows = dend_rows, dend_cols = dend_cols,
                  dend_rows_side = dend_rows_side, dend_cols_side = dend_cols_side,
                  dend_col = dend_col, dend_height = dend_height, dend_lwd = dend_lwd, dend_lty = dend_lty,
                  dend_rows_params = dend_rows_params, dend_cols_params = dend_cols_params,
                  dend_rows_extend = dend_rows_extend, dend_cols_extend = dend_cols_extend,
                  legend_position = legend_position, plot_margin = plot_margin, margin_unit = margin_unit)

  # cor_plt <- gghm(cor_mat, fill_scale = fill_scale, fill_name = fill_name, ...)

  return(cor_plt)
}


