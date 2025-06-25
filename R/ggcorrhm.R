#' Make a correlation heatmap with ggplot2.
#'
#' Make a correlation heatmap from input matrices. Uses a diverging colour scale centered around 0.
#'
#' @inheritParams gghm
#'
#' @param x Matrix or data frame in wide format containing the columns to correlate against each other or against the columns in `y`.
#' @param y Optional matrix or data frame in wide format containing columns to correlate with the columns in `x`.
#' @param cor_method String specifying correlation method to use in the `stats::cor()` function. Default is 'pearson'.
#' @param cor_use String specifying the `use` argument of `stats::cor()`, which defines how to deal with missing values. Default is 'everything'.
#' @param high Colour to use for the highest value of the colour scale.
#' @param mid Colour to use for 0 in the colour scale.
#' @param low Colour to use for the lowest value of the colour scale.
#' @param limits Correlation limits to plot between.
#' @param bins Specify number of bins if the correlation scale should be binned. NULL for a continuous scale.
#' @param fill_name String to use for the correlation fill scale. If NULL (default) the text will depend on the correlation method.
#' @param col_name String to use for the correlation colour scale. If NULL (default) the text will depend on the correlation method.
#' @param p_values Logical indicating if p-values should be calculated. Use with `p_thresholds` to mark cells, and/or `return_data` to get the p-values in the output data.
#' @param p_adjust String specifying the adjustment method to use for the p-values (default is "none").
#' @param p_thresholds Named numeric vector specifying p-value thresholds (in ascending order) to mark. The last element must be 1 or higher (to set the upper limit).
#' Names must be unique, but one element can be left unnamed (by default 1 is unnamed, meaning values between the threshold closest to 1 and 1 are not marked in the plot).
#' @param layout String specifying the layout of the output heatmap. Possible layouts include
#' 'topleft', 'topright', 'bottomleft', 'bottomright', or the 'whole'/'full' heatmap (default and only possible option if the matrix is asymmetric).
#' A combination of the first letters of each word also works (i.e. f, w, tl, tr, bl, br).
#' If layout is of length two with two opposing triangles, a mixed layout will be used. For mixed layouts,
#' `mode` needs a vector of length two (applied in the same order as layout). See details of `gghm()` for more information.
#' @param include_diag Logical indicating if the diagonal cells should be plotted (if the matrix is symmetric).
#' @param na_remove Logical indicating if NA values in the heatmap should be omitted (meaning no cell border is drawn). This does not affect how
#' NAs are handled in the correlation computations, use the `cor_use` argument for NA handling in correlation.
#' @param na_col Colour to use for cells with NA.
#' @param return_data Logical indicating if the data used for plotting (i.e. the correlation values and, if computed, clustering and p-values) should be returned.
#' @param size_range Numeric vector of length 2, specifying lower and upper ranges of shape sizes. Ignored if `size_scale` is not NULL.
#' @param size_scale `ggplot2::scale_size_*` call to use for size scaling if `mode` is a number from 1 to 25 (R pch).
#' The default behaviour (NULL) is to use a continuous scale with the absolute values of the correlation.
#' @param cell_labels Logical specifying if the cells should be labelled with the correlation values.
#' @param cell_label_p Logical indicating if, when `cell_labels` is `TRUE`, p-values should be written instead of correlation values.
#' @param cell_label_col Colour to use for cell labels, passed to `ggplot2::geom_text()`.
#' @param cell_label_size Size of cell labels, used as the `size` argument in `ggplot2::geom_text()`.
#' @param annot_rows_params Named list with parameters for row annotations to overwrite the defaults set by the `annot_*` arguments, each name corresponding to the `*` part
#' (see details of `gghm()` for more information).
#' @param dend_rows_params Named list for row dendrogram parameters. See details of `gghm()` for more information.
#' @param dend_cols_params Named list for column dendrogram parameters. See details of `gghm()` for more information.
#' @param dend_rows_extend Named list or functional sequence for specifying `dendextend` functions to apply to the row dendrogram. See details of `gghm()` for usage.
#' @param dend_cols_extend Named list or functional sequence for specifying `dendextend` functions to apply to the column dendrogram. See details of `gghm()` for usage.
#'
#' @return The correlation heatmap as a `ggplot` object.
#' If `return_data` is TRUE the output is a list containing the plot (named 'plot'),
#' the correlations ('plot_data', with factor columns 'row' and 'col' and a column 'value' containing the cell values),
#' and the result of the clustering ('row_clustering' and 'col_clustering', if clustered).
#' If p-values were calculated, two additional columns named 'p_val' and 'p_adj' are included in 'plot_data', containing nominal and adjusted p-values.
#' If the layout is mixed, an extra factor column named 'layout' is included, showing which triangle each cell belongs to.
#'
#' @export
#'
#' @details
#'
#' `ggcorrhm()` makes it convenient to make correlation heatmaps, taking the input matrix or data frame to visualise the correlations between columns with the `gghm()` function.
#' The input values can either be one matrix or data frame with columns to correlate with each other, or two
#' matrices or data frames with columns to correlate between the matrices. No rownames are needed, but
#' if two matrices are provided they should have the same number of rows and the rows should be ordered in a meaningful way
#' (i.e. same sample/individual/etc in the same row in both).
#'
#' Row and column names are displayed in the diagonal by default if the correlation matrix is symmetric (only `x` is provided or `x` and `y` are identical).
#'
#' The colour scale is set to be a diverging gradient around 0, with options to change the low, mid, and high colours and the limits.
#' The `bins` argument converts the scale to a discrete scale divided into `bins` equally distributed bins.
#'
#' The size scale, used when a numeric cell shape is specified, is set to vary the shape size between 4 and 10 (can be changed with the `size_range` argument)
#' and to transform the values to absolute values (so that both positive and negative correlations are treated equally).
#' This behaviour can be overwritten by setting `size_scale` to another `ggplot2::scale_size_*` function with the desired
#' arguments, or `ggplot2::scale_size()` for no special behaviour.
#' When the absolute value transformation is used the legend for sizes loses its meaning (only displaying positive values)
#' and is therefore set to not be shown in the `show_legend` argument.
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
#' ggcorrhm(mtcars, layout = "tl", cluster_rows = TRUE, cluster_cols = TRUE)
#'
#' # With annotation
#' set.seed(123)
#' annot <- data.frame(.names = colnames(mtcars),
#'                     annot1 = rnorm(ncol(mtcars)),
#'                     annot2 = sample(letters[1:3], ncol(mtcars), TRUE))
#' ggcorrhm(mtcars, layout = "tr", annot_cols_df = annot)
ggcorrhm <- function(x, y = NULL, cor_method = "pearson", cor_use = "everything",
                     high = "sienna2", mid = "white", low = "skyblue2",
                     limits = c(-1, 1), bins = NULL, fill_name = NULL, col_name = fill_name,
                     p_values = FALSE, p_adjust = "none", p_thresholds = c("***" = 0.001, "**" = 0.01, "*" = 0.05, 1),
                     mode = if (length(layout) == 1) "heatmap" else c("heatmap", "text"),
                     layout = "full", include_diag = TRUE, na_remove = FALSE, na_col = "grey", return_data = FALSE,
                     show_legend = c("fill" = TRUE, "colour" = FALSE, "size" = FALSE),
                     size_range = c(4, 10), size_scale = NULL,
                     cell_labels = FALSE, cell_label_p = FALSE, cell_label_col = "black", cell_label_size = 3, cell_label_digits = 2,
                     border_col = "grey", border_lwd = 0.5, border_lty = 1,
                     names_diag = TRUE, names_diag_param = NULL,
                     names_x = FALSE, names_x_side = "top", names_y = FALSE, names_y_side = "left",
                     annot_rows_df = NULL, annot_cols_df = NULL, annot_rows_fill = NULL, annot_cols_fill = NULL,
                     annot_rows_side = "right", annot_cols_side = "bottom",
                     annot_legend = TRUE, annot_dist = 0.2, annot_gap = 0, annot_size = 0.5, annot_label = TRUE,
                     annot_border_col = if (length(border_col) == 1) border_col else "grey",
                     annot_border_lwd = if (length(border_lwd) == 1) border_lwd else 0.5,
                     annot_border_lty = if (length(border_lty) == 1) border_lty else 1,
                     annot_na_col = "grey", annot_na_remove = na_remove,
                     annot_rows_params = NULL, annot_cols_params = NULL,
                     annot_rows_label_side = "bottom", annot_cols_label_side = "left",
                     annot_rows_label_params = NULL, annot_cols_label_params = NULL,
                     cluster_rows = FALSE, cluster_cols = FALSE,
                     cluster_distance = "euclidean", cluster_method = "complete",
                     dend_rows = TRUE, dend_cols = TRUE, dend_rows_side = "right", dend_cols_side = "bottom",
                     dend_col = "black", dend_height = 0.3, dend_lwd = 0.3, dend_lty = 1,
                     dend_rows_params = NULL, dend_cols_params = NULL,
                     dend_rows_extend = NULL, dend_cols_extend = NULL) {

  # If p-values are computed and thresholds given, check that they range between 0 and 1 and that they have enough names
  if (any(unlist(p_values)) & is.numeric(p_thresholds)) {
    if (any(p_thresholds < 0)) cli::cli_abort("Values in {.var p_thresholds} must be above 0.", class = "p_thr_error")
    if (p_thresholds[length(p_thresholds)] < 1) cli::cli_abort("The last value of {.var p_thresholds} must be 1 or larger.", class = "p_thr_error")
    if (is.null(names(p_thresholds))) cli::cli_abort("{.var p_thresholds} must have named elements to be used as symbols (up to one unnamed).", class = "p_thr_error")
    if (any(duplicated(names(p_thresholds)))) cli::cli_abort("Symbols (the names) of {.var p_thresholds} must be unique.", class = "p_thr_error")
  }

  # Save input mode in case it is changed
  mode_og <- mode
  # If text mode, switch to none and add text later
  if ("text" %in% mode) {
    mode[mode == "text"] <- "none"
  }

  cor_mat <- if (is.null(y)) {
    cor(x, method = cor_method, use = cor_use)
  } else {
    cor(x, y, method = cor_method, use = cor_use)
  }

  if (!any(unlist(p_values))) {
    if (is.null(y)) {
      cor_mat <- cor(x, method = cor_method, use = cor_use)
    } else {
      cor_mat <- cor(x, y, method = cor_method, use = cor_use)
    }
  } else {
    cor_mat <- test_cor(x, y, full_plt = any(c("full", "f", "whole", "w") %in% layout),
                        method = cor_method, use = cor_use, p_adj_method = p_adjust)
    # Keep the data for later plotting
    cor_mat_dat <- cor_mat
    # Get wide format correlation matrix
    cor_mat <- reshape(dplyr::select(cor_mat, -"p_val", -"p_adj"), idvar = "row", timevar = "col", direction = "wide")
    rownames(cor_mat) <- cor_mat[["row"]]
    cor_mat <- cor_mat[, -which(colnames(cor_mat) == "row")]
    # Remove "value." from colnames
    cor_mat <- dplyr::rename_with(cor_mat, function(nm) {substring(nm, 7)})
    cor_mat <- as.matrix(cor_mat)
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
  show_fill <- if ("fill" %in% names(show_legend)) show_legend["fill"] else show_legend
  show_col <- if ("colour" %in% names(show_legend)) {
    show_legend["colour"]
  } else if ("color" %in% names(show_legend)) {
    show_legend["color"]
  } else {
    show_legend
  }
  fill_scale <- if (!is.null(bins)) {
    ggplot2::scale_fill_steps2(limits = limits, high = high, mid = mid, low = low, na.value = na_col,
                               breaks = seq(limits[1], limits[2], length.out = bins),
                               guide = if (show_fill) ggplot2::guide_colourbar(order = 1) else "none")
  } else {
    ggplot2::scale_fill_gradient2(limits = limits, high = high, mid = mid, low = low, na.value = na_col,
                                  guide = if (show_fill) ggplot2::guide_colourbar(order = 1) else "none")
  }
  col_scale <- if (!is.null(bins)) {
    ggplot2::scale_colour_steps2(limits = limits, high = high, mid = mid, low = low, na.value = na_col,
                                 breaks = seq(limits[1], limits[2], length.out = bins),
                                 guide = if (show_col) ggplot2::guide_colourbar(order = 1) else "none")
  } else {
    ggplot2::scale_colour_gradient2(limits = limits, high = high, mid = mid, low = low, na.value = na_col,
                                    guide = if (show_col) ggplot2::guide_colourbar(order = 1) else "none")
  }

  # Make size scale (controlling the size range and transforming to be absolute values)
  if (any(1:25 %in% mode) & is.null(size_scale)) {
    size_scale <- ggplot2::scale_size_continuous(range = size_range,
                                                 transform = scales::trans_new("abs", abs, abs))
  }

  # Call with all arguments to get the tooltips when calling this wrapper function
  cor_plt <- gghm(cor_mat, fill_scale = fill_scale, fill_name = fill_name,
                  col_scale = col_scale, col_name = col_name, na_remove = na_remove,
                  mode = mode, layout = layout, include_diag = include_diag, return_data = T,
                  show_legend = show_legend, size_scale = size_scale,
                  border_col = border_col, border_lwd = border_lwd, border_lty = border_lty,
                  names_diag = names_diag, names_diag_param = names_diag_param,
                  names_x = names_x, names_x_side = names_x_side,
                  names_y = names_y, names_y_side = names_y_side,
                  annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df,
                  annot_rows_fill = annot_rows_fill, annot_cols_fill = annot_cols_fill,
                  annot_rows_side = annot_rows_side, annot_cols_side = annot_cols_side,
                  annot_legend = annot_legend, annot_dist = annot_dist, annot_gap = annot_gap,
                  annot_size = annot_size, annot_label = annot_label,
                  annot_border_col = annot_border_col, annot_border_lwd = annot_border_lwd, annot_border_lty = annot_border_lty,
                  annot_na_col = "grey", annot_na_remove = annot_na_remove,
                  annot_rows_params = annot_rows_params, annot_cols_params = annot_cols_params,
                  annot_rows_label_side = annot_rows_label_side, annot_cols_label_side = annot_cols_label_side,
                  annot_rows_label_params = annot_rows_label_params, annot_cols_label_params = annot_cols_label_params,
                  cluster_rows = cluster_rows, cluster_cols = cluster_cols,
                  cluster_distance = cluster_distance, cluster_method = cluster_method,
                  dend_rows = dend_rows, dend_cols = dend_cols,
                  dend_rows_side = dend_rows_side, dend_cols_side = dend_cols_side,
                  dend_col = dend_col, dend_height = dend_height, dend_lwd = dend_lwd, dend_lty = dend_lty,
                  dend_rows_params = dend_rows_params, dend_cols_params = dend_cols_params,
                  dend_rows_extend = dend_rows_extend, dend_cols_extend = dend_cols_extend)

  # Prepare parameters for two-length layouts (separate from what is done in gghm)
  if (length(layout) == 2) {
    border_col <- prepare_mixed_param(border_col, "border_col")
    border_lwd <- prepare_mixed_param(border_lwd, "border_lwd")
    border_lty <- prepare_mixed_param(border_lty, "border_lty")
    cell_labels <- prepare_mixed_param(cell_labels, "cell_labels")
    cell_label_col <- prepare_mixed_param(cell_label_col, "cell_label_col")
    cell_label_size <- prepare_mixed_param(cell_label_size, "cell_label_size")
    cell_label_digits <- prepare_mixed_param(cell_label_digits, "cell_label_digits")
    p_values <- prepare_mixed_param(p_values, "p_values")
    cell_label_p <- prepare_mixed_param(cell_label_p, "cell_label_p")
  }

  # Add p-values and cell labels
  if ("text" %in% mode_og | any(unlist(p_values)) | any(unlist(cell_labels))) {
    if (length(layout) == 1) {
      cor_plt_lab <- add_pvalue_labels(cor_mat_dat = if (p_values) {cor_mat_dat} else {NULL},
                                       cor_plt_dat = cor_plt[["plot_data"]], cor_plt_plt = cor_plt[["plot"]],
                                       mode = mode_og, skip_diag = isSymmetric(cor_mat) & names_diag,
                                       cell_labels = cell_labels, cell_label_col = cell_label_col,
                                       cell_label_size = cell_label_size, cell_label_digits = cell_label_digits,
                                       cell_label_p = cell_label_p, p_thresholds = p_thresholds,
                                       border_col = border_col,border_lwd = border_lwd, border_lty = border_lty,
                                       show_legend = show_legend, col_scale = col_scale)
      cor_plt[["plot"]] <- cor_plt_lab[["plot"]]
      cor_plt[["plot_data"]] <- cor_plt_lab[["plot_data"]]
    } else if (length(layout) == 2) {
      # Avoid name clash
      lt <- layout
      # First half
      p_plt1 <- add_pvalue_labels(cor_mat_dat = if (p_values[[1]]) {cor_mat_dat} else {NULL},
                                  cor_plt_dat = subset(cor_plt[["plot_data"]], layout == lt[[1]]),
                                  cor_plt_plt = cor_plt[["plot"]], mode = mode_og[[1]],
                                  skip_diag = isSymmetric(cor_mat) & names_diag, cell_labels = cell_labels[[1]],
                                  cell_label_col = cell_label_col[[1]], cell_label_size = cell_label_size[[1]],
                                  cell_label_digits = cell_label_digits[[1]], cell_label_p = cell_label_p[[1]],
                                  p_thresholds = p_thresholds, border_col = border_col[[1]], border_lwd = border_lwd[[1]],
                                  border_lty = border_lty[[1]], show_legend = show_legend, col_scale = col_scale)

      # Second half, use the returned plot
      p_plt2 <- add_pvalue_labels(cor_mat_dat = if (p_values[[2]]) {cor_mat_dat} else {NULL},
                                  cor_plt_dat = subset(cor_plt[["plot_data"]], layout == lt[[2]]),
                                  cor_plt_plt = p_plt1[["plot"]], mode = mode_og[[2]],
                                  skip_diag = isSymmetric(cor_mat) & names_diag, cell_labels = cell_labels[[2]],
                                  cell_label_col = cell_label_col[[2]], cell_label_size = cell_label_size[[2]],
                                  cell_label_digits = cell_label_digits[[2]], cell_label_p = cell_label_p[[2]],
                                  p_thresholds = p_thresholds, border_col = border_col[[2]], border_lwd = border_lwd[[2]],
                                  border_lty = border_lty[[2]], show_legend = show_legend, col_scale = col_scale)

      # Since only the plotted part of the data is returned, bind them together
      cor_plt[["plot"]] <- p_plt2[["plot"]]
      cor_plt[["plot_data"]] <- dplyr::bind_rows(p_plt1[["plot_data"]], p_plt2[["plot_data"]])
    }
  }

  cor_out <- if (return_data) {cor_plt} else {cor_plt[["plot"]]}
  return(cor_out)
}


#' Helper function to add p-value labels for ggcorrhm
#'
#' @keywords internal
#'
#' @param cor_mat_dat Correlation matrix data with p-values (leave as NULL if not computed).
#' @param cor_plt_dat Plot data from gghm.
#' @param cor_plt_plt Plot from gghm.
#' @param mode The plotting mode.
#' @param skip_diag If the diagonal should be skipped (to not draw on names).
#' @param cell_labels Logical, if labels should be drawn.
#' @param cell_label_col Cell label colours.
#' @param cell_label_size Cell label sizes.
#' @param cell_label_digits Cell label digits.
#' @param p_thresholds P-value thresholds.
#' @param border_col Cell border colours.
#' @param border_lwd Cell border line widths.
#' @param border_lty Cell border line types.
#' @param show_legend Vector indicating which legends should be shown.
#'
#' @returns Plot with labels added.
#'
add_pvalue_labels <- function(cor_mat_dat = NULL, cor_plt_dat, cor_plt_plt, mode, skip_diag = F,
                              cell_labels, cell_label_p, cell_label_col, cell_label_size, cell_label_digits,
                              p_thresholds, border_col, border_lwd, border_lty, show_legend, col_scale) {

  if (cell_label_p & is.null(cor_mat_dat)) {
    cli::cli_warn("{.var cell_label_p} is {.val TRUE} but {.var p_values} is {.val FALSE}.
                  Writing correlation values as no p-values have been computed.",
                  class = "cell_label_p_warn")
  }

  label_df <- cor_plt_dat
  label_df[["label"]] <- round(label_df[["value"]], cell_label_digits)

  # Add p-values if computed
  if (!is.null(cor_mat_dat)) {
    lab_name <- ifelse(cell_label_p, "p_adj", "value")

    cor_plt_dat <- dplyr::left_join(cor_plt_dat, cor_mat_dat, by = c("row", "col", "value"))
    if (is.numeric(p_thresholds)) {
      # Convert p-values to symbols
      label_df <- cor_plt_dat
      label_df[["p_sym"]] <- as.character(symnum(x = label_df[["p_adj"]],
                                                 # Add 0 to set the range of possible values
                                                 cutpoints = c(0, p_thresholds),
                                                 symbols = names(p_thresholds)))
      if (cell_labels | mode == "text") {
        # Add symbols to labels if both p-values and labels
        label_df[["label"]] <- paste0(round(label_df[[lab_name]], cell_label_digits), label_df[["p_sym"]])
      } else {
        # Just plot the symbols if no labels
        label_df[["label"]] <- label_df[["p_sym"]]
      }
    }
  }

  # Add text mode text
  if (mode == "text") {
    cor_plt_plt <- add_text_geom(dat = label_df, plt = cor_plt_plt, type = "text_mode",
                                 show_legend = show_legend, skip_diag = skip_diag, col_scale = col_scale,
                                 cell_label_col = cell_label_col, cell_label_size = cell_label_size,
                                 border_col = border_col, border_lwd = border_lwd, border_lty = border_lty)
  }

  # Write the cell labels (don't overwrite if cell_labels is FALSE)
  if (cell_labels | (!is.null(cor_mat_dat) & is.numeric(p_thresholds) & mode != "text")) {
    cor_plt_plt <- add_text_geom(dat = label_df, plt = cor_plt_plt, type = "cell_label",
                                 show_legend = show_legend, skip_diag = skip_diag, col_scale = col_scale,
                                 cell_label_col = cell_label_col, cell_label_size = cell_label_size,
                                 border_col = border_col, border_lwd = border_lwd, border_lty = border_lty)
  }

  return(list(plot = cor_plt_plt, plot_data = cor_plt_dat))
}

#' Helper function for adding the text geom depending on mode/cell_labels
#'
#' @keywords internal
#'
#' @param dat Plotting data.
#' @param plt Plot to add to.
#' @param type String saying if it is 'text_mode' or 'cell_label' that is being drawn (changes scaling of colours).
#' @param show_legend Legends to be shown.
#' @param skip_diag If the diagonal should be skipped.
#' @param cell_label_col Cell label colours.
#' @param cell_label_size Cell label sizes.
#' @param border_col Cell border colours.
#' @param border_lwd Cell border line widths.
#' @param border_lty Cell border line types.
#'
#' @returns The plot with the text geom added.
#'
add_text_geom <- function(dat, plt, type = c("text_mode", "cell_label"), show_legend, skip_diag,
                          cell_label_col, cell_label_size, border_col, border_lwd, border_lty, col_scale) {
  label <- value <- NULL

  if (type == "text_mode") {
    # Text, colour scaling with value and with tiles around
    plt <- plt +
      ggnewscale::new_scale_colour() +
      ggplot2::geom_text(
        ggplot2::aes(label = label, colour = value),
        data = if (skip_diag) {
          subset(dat, as.character(row) != as.character(col))
        } else {dat}, size = cell_label_size,
        show.legend = show_legend
      ) +
      ggplot2::geom_tile(data = dat, linewidth = border_lwd, colour = border_col,
                         linetype = border_lty, alpha = 0) +
      col_scale
  } else if (type == "cell_label") {
    # Cell labels
    plt <- plt +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        data = if (skip_diag) {
          subset(dat, as.character(row) != as.character(col))
        } else {dat}, size = cell_label_size, colour = cell_label_col
      )
  }

  return(plt)
}
