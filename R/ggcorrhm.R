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
#' @param cor_in Logical indicating if the input data contains correlation values and any correlation computations (including p-values) should be skipped. Default is FALSE.
#' @param high Colour to use for the highest value of the colour scale.
#' @param mid Colour to use for 0 in the colour scale.
#' @param low Colour to use for the lowest value of the colour scale.
#' @param midpoint Value for the middle point of the colour scale.
#' @param col_scale Scale to use for cell colours. If NULL (default), a divergent scale is constructed from the `high`, `mid`, `low`, `midpoint`, `limits`, and `bins` arguments.
#' These arguments are ignored if a `ggplot2::scale_*` function is provided instead. If a string, the corresponding Brewer or Viridis scale is used.
#' A string with a scale name with "rev_" in the beginning or "_rev" at the end will result in the reversed scale.
#' In mixed layouts, can also be a list of length two containing the two scales to use.
#' @param col_name String to use for the correlation scale. If NULL (default) the text will depend on the correlation method. Can be two values in mixed layouts for dual scales.
#' @param p_values Logical indicating if p-values should be calculated. Use with `p_thresholds` to mark cells, and/or `return_data` to get the p-values in the output data.
#' @param p_adjust String specifying the multiple testing adjustment method to use for the p-values (default is "none"). Passed to `stats::p.adjust()`.
#' @param p_thresholds Named numeric vector specifying p-value thresholds (in ascending order) to mark. The last element must be 1 or higher (to set the upper limit).
#' Names must be unique, but one element can be left unnamed (by default 1 is unnamed, meaning values between the threshold closest to 1 and 1 are not marked in the plot).
#' If NULL, no thresholding is done and p-value intervals are not marked with symbols.
#' @param layout String specifying the layout of the output heatmap. Possible layouts include
#' 'topleft', 'topright', 'bottomleft', 'bottomright', or the 'whole'/'full' heatmap (default and only possible option if the matrix is asymmetric).
#' A combination of the first letters of each word also works (i.e. f, w, tl, tr, bl, br).
#' If layout is of length two with two opposing triangles, a mixed layout will be used. For mixed layouts,
#' `mode` needs a vector of length two (applied in the same order as layout). See details of `gghm()` for more information.
#' @param include_diag Logical indicating if the diagonal cells should be plotted (if the matrix is symmetric).
#' @param na_remove Logical indicating if NA values in the heatmap should be omitted (meaning no cell border is drawn). This does not affect how
#' NAs are handled in the correlation computations, use the `cor_use` argument for NA handling in correlation.
#' @param return_data Logical indicating if the data used for plotting (i.e. the correlation values and, if computed, clustering and p-values) should be returned.
#' @param legend_order Integer vector specifying the order of legends (first value is for the first legend, second for the second, etc). The default (NULL) shows all but size legends.
#' NAs hide the corresponding legends, a single NA hides all. Ignored for `ggplot2` scale objects in `col_scale` and `size_scale`.
#' @param size_range Numeric vector of length 2, specifying lower and upper ranges of shape sizes. Ignored if `size_scale` is not NULL.
#' @param size_scale `ggplot2::scale_size_*` call to use for size scaling if `mode` is a number from 1 to 25 (R pch).
#' The default behaviour (NULL) is to use a continuous scale with the absolute values of the correlation.
#' @param cell_labels Logical specifying if the cells should be labelled with the correlation values. Alternatively, a matrix or data frame with the same shape and dimnames as `x`
#' containing values to write in the cells. If mode is `text`, the cell label colours will scale with the correlation values and `cell_label_col` is ignored.
#' @param cell_label_p Logical indicating if, when `cell_labels` is `TRUE`, p-values should be written instead of correlation values.
#' @param cell_label_col Colour to use for cell labels, passed to `ggplot2::geom_text()`.
#' @param cell_label_size Size of cell labels, used as the `size` argument in `ggplot2::geom_text()`.
#' @param annot_rows_params,annot_cols_params Named list with parameters for row or column annotations to overwrite the defaults set by the `annot_*` arguments, each name corresponding to the `*` part
#' (see details of `gghm()` for more information).
#' @param dend_rows_params,dend_cols_params Named list for row or column dendrogram parameters. See details of `gghm()` for more information.
#' @param dend_rows_extend,dend_cols_extend Named list or functional sequence for specifying `dendextend` functions to apply to the row or column dendrogram. See details of `gghm()` and `ggcorrhm()` for usage.
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
#' The colour scale is set to be a diverging gradient around 0, with options to change the `low`, `mid`, and `high` colours, the `midpoint`, and the `limits` (using the arguments
#' of the same names). The `bins` argument converts the scale to a discrete scale divided into `bins` equally distributed bins (if an integer the breaks may be at strange numbers,
#' if a double the number of bins may be different but the breaks are at nicer numbers). These arguments can be of length two (`limits` a list of length two) two apply
#' to each triangle in a mixed layout (detailed more in the details section of `gghm()`). The `size_range` argument (for size scales) can also be a list of length two like `limits`.
#'
#' The size scale, used when a numeric cell shape is specified, is set to vary the shape size between 4 and 10 (can be changed with the `size_range` argument)
#' and to transform the values to absolute values (so that both positive and negative correlations are treated equally).
#' This behaviour can be overwritten by setting `size_scale` to another `ggplot2::scale_size_*` function with the desired
#' arguments, or `ggplot2::scale_size()` for no special behaviour. `ggplot2::scale_size_area()` also scales with the absolute value,
#' but only the upper size limit can be set.
#' When the absolute value transformation is used the legend for sizes loses its meaning (only displaying positive values)
#' and is therefore set to not be shown if `legend_order` is NULL.
#'
#' For symmetric correlation matrices, the dendrogram customisation arguments `dend_rows_extend` and `dend_cols_extend` work best with functions that only change the dendrogram
#' cosmetically such as the colours, linetypes or node shapes. While it is possible to reorder (using e.g. 'rotate', 'ladderize') or prune (using e.g. 'prune'),
#' anything that changes the structure of the dendrogram may end up looking strange for symmetric matrices if
#' only applied to one dimension (e.g. the diagonal may not be on the diagonal, triangular or mixed layouts may not work).
#' The same applies if the `cluster_rows` and `cluster_cols` arguments are `hclust` or `dendrogram` objects.
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
#'
#' # Both
#' ggcorrhm(mtcars, layout = "full", cluster_rows = TRUE, cluster_cols = TRUE,
#'          annot_rows_df = annot[, -3], annot_cols_df = annot[, -2])
#'
#' # Mixed layout
#' ggcorrhm(mtcars, layout = c("tl", "br"))
#'
ggcorrhm <- function(x, y = NULL, cor_method = "pearson", cor_use = "everything", cor_in = FALSE,
                     high = "sienna2", mid = "white", low = "skyblue2", midpoint = 0, limits = c(-1, 1), bins = NULL,
                     layout = "full", mode = if (length(layout) == 1) "heatmap" else c("heatmap", "text"),
                     include_diag = TRUE, na_col = "grey50", na_remove = FALSE, return_data = FALSE,
                     col_scale = NULL, col_name = NULL,
                     size_range = c(4, 10), size_scale = NULL, size_name = NULL,
                     legend_order = NULL,
                     p_values = FALSE, p_adjust = "none", p_thresholds = c("***" = 0.001, "**" = 0.01, "*" = 0.05, 1),
                     cell_labels = FALSE, cell_label_p = FALSE, cell_label_col = "black", cell_label_size = 3, cell_label_digits = 2,
                     cell_bg_col = "white", cell_bg_alpha = 0,
                     border_col = "grey", border_lwd = 0.1, border_lty = 1,
                     show_names_diag = TRUE, names_diag_params = NULL,
                     show_names_x = FALSE, names_x_side = "top", show_names_y = FALSE, names_y_side = "left",
                     annot_rows_df = NULL, annot_cols_df = NULL,
                     annot_rows_col = NULL, annot_cols_col = NULL,
                     annot_rows_side = "right", annot_cols_side = "bottom",
                     annot_dist = 0.2, annot_gap = 0, annot_size = 0.5,
                     annot_border_col = if (length(border_col) == 1) border_col else "grey",
                     annot_border_lwd = if (length(border_lwd) == 1) border_lwd else 0.5,
                     annot_border_lty = if (length(border_lty) == 1) border_lty else 1,
                     annot_na_col = na_col, annot_na_remove = na_remove,
                     annot_rows_params = NULL, annot_cols_params = NULL,
                     show_annot_names = TRUE, annot_names_size = 3,
                     annot_rows_names_side = "bottom", annot_cols_names_side = "left",
                     annot_rows_names_params = NULL, annot_cols_names_params = NULL,
                     annot_rows_name_params = NULL, annot_cols_name_params = NULL,
                     cluster_rows = FALSE, cluster_cols = FALSE,
                     cluster_distance = "euclidean", cluster_method = "complete",
                     show_dend_rows = TRUE, show_dend_cols = TRUE, dend_rows_side = "right", dend_cols_side = "bottom",
                     dend_col = "black", dend_dist = 0, dend_height = 0.3, dend_lwd = 0.3, dend_lty = 1,
                     dend_rows_params = NULL, dend_cols_params = NULL,
                     dend_rows_extend = NULL, dend_cols_extend = NULL,
                     split_rows = NULL, split_cols = NULL) {

  # Perform some input argument checks
  check_logical(return_data = return_data)
  check_layout(layout, mode)

  if (length(mode) > 1) {
    check_logical(p_values = p_values, list_allowed = TRUE)
    check_logical(cell_label_p = cell_label_p, list_allowed = TRUE)
  } else {
    check_logical(p_values = p_values, list_allowed = FALSE)
    check_logical(cell_label_p = cell_label_p, list_allowed = FALSE)
  }

  if (any(unlist(p_values))) {
    if (!is.numeric(p_thresholds) && !is.null(p_thresholds)) {
      cli::cli_abort("{.var p_thresholds} must be a named {.cls numeric} vector or NULL.",
                     class = "p_thr_class_error")
    } else if (!is.null(p_thresholds)) {
      if (any(is.na(p_thresholds))) cli::cli_abort("{.var p_thresholds} should not contain any missing values.", class = "p_thr_error")
      if (any(p_thresholds <= 0)) cli::cli_abort("Values in {.var p_thresholds} must be above 0.", class = "p_thr_error")
      if (p_thresholds[length(p_thresholds)] < 1) cli::cli_abort("The last value of {.var p_thresholds} must be 1 or larger.", class = "p_thr_error")
      if (is.null(names(p_thresholds))) cli::cli_abort("{.var p_thresholds} must have named elements to be used as symbols (up to one unnamed).", class = "p_thr_error")
      if (any(duplicated(names(p_thresholds)))) cli::cli_abort("Symbols (the names) of {.var p_thresholds} must be unique.", class = "p_thr_error")
    }
  }


  # Skip correlation (and p-value) computation if cor_in is TRUE
  check_logical(cor_in = cor_in)
  if (isTRUE(cor_in)) {
    cor_mat <- x
    cor_mat_dat <- NULL
    p_values <- FALSE
    cell_label_p <- FALSE

  } else {
    cor_mat <- if (is.null(y)) {
      cor(x, method = cor_method, use = cor_use)
    } else {
      cor(x, y, method = cor_method, use = cor_use)
    }

    # Placeholder for p-value data that may be replaced
    cor_mat_dat <- NULL
    if (!any(unlist(p_values))) {
      if (is.null(y)) {
        cor_mat <- cor(x, method = cor_method, use = cor_use)
      } else {
        cor_mat <- cor(x, y, method = cor_method, use = cor_use)
      }
    } else {
      cor_mat <- test_cor(x, y, method = cor_method, use = cor_use, p_adj_method = p_adjust)
      # Keep the data for later plotting
      cor_mat_dat <- cor_mat

      # Get wide format correlation matrix
      cor_mat <- shape_mat_wide(dplyr::select(cor_mat, -"p_val", -"p_adj"))
    }
  }

  # Check annotation data frames (must be done before making scales)
  if (!is.null(annot_rows_df)) {
    annot_rows_df <- check_annot_df(annot_rows_df, rownames(cor_mat), "annot_rows_df")
  }
  if (!is.null(annot_cols_df)) {
    annot_cols_df <- check_annot_df(annot_cols_df, colnames(cor_mat), "annot_cols_df")
  }

  # Prepare parameters for two-length layouts (separate from what is done in gghm)
  if (length(layout) == 2) {
    p_values <- prepare_mixed_param(p_values, "p_values")
    cell_labels <- prepare_mixed_param(cell_labels, "cell_labels")
    cell_label_p <- prepare_mixed_param(cell_label_p, "cell_label_p")
    cell_label_digits <- prepare_mixed_param(cell_label_digits, "cell_label_digits")
  }

  # Prepare cell labels
  cell_labels <- prepare_cell_labels(mode, cell_labels, p_values, cell_label_p,
                                     cell_label_digits, p_thresholds, cor_mat_dat)

  # Name of fill legend
  scale_names <- switch(cor_method,
                        "pearson" = "Pearson r",
                        "spearman" = "Spearman\nrho",
                        "kendall" = "Kendall\ntau")
  if (is.null(col_name)) {
    col_name <- scale_names
  }
  if (is.null(size_name)) {
    size_name <- scale_names
  }

  # Don't display names on the diagonal if the plot is non-symmetric as it will cause
  # new ghost columns to be added to draw the names where row == col
  if (!isSymmetric(as.matrix(cor_mat))) {
    show_names_diag <- FALSE
    # Also display x and y names by default, but remove if specified as FALSE (when specified as a named argument)
    show_names_x <- eval(replace_default(list("show_names_x" = TRUE), as.list(sys.call()))$show_names_x)
    show_names_y <- eval(replace_default(list("show_names_y" = TRUE), as.list(sys.call()))$show_names_y)
  }

  # Get scales and their orders
  scale_order <- make_legend_order(mode = mode,
                                   col_scale = col_scale,
                                   size_scale = size_scale, annot_rows_df = annot_rows_df,
                                   annot_cols_df = annot_cols_df,
                                   bins = bins, limits = limits,
                                   high = high, mid = mid, low = low, na_col = na_col,
                                   midpoint = midpoint, size_range = size_range,
                                   legend_order = legend_order)

  # Prepare scales for mixed layouts
  if (length(layout) == 2) {
    bins <- prepare_mixed_param(bins, "bins")
    limits <- prepare_mixed_param(limits, "limits")
    midpoint <- prepare_mixed_param(midpoint, "midpoint")
    size_range <- prepare_mixed_param(size_range, "size_range")
    high <- prepare_mixed_param(high, "high")
    mid <- prepare_mixed_param(mid, "mid")
    low <- prepare_mixed_param(low, "low")
    na_col <- prepare_mixed_param(na_col, "na_col")
    col_name <- prepare_mixed_param(col_name, "col_name")
    col_scale <- prepare_mixed_param(col_scale, "col_scale")
    size_name <- prepare_mixed_param(size_name, "size_name")
    size_scale <- prepare_mixed_param(size_scale, "size_scale")
  }

  # Hide some legends by default in the main plot
  if (is.null(legend_order)) {

    # Hide all size legends, default is transformed without a way to map back to values (abs)
    if ("size" %in% scale_order[["main_scales"]][["scales"]]) {
      scale_order[["main_scales"]][["order"]][which(scale_order[["main_scales"]][["scales"]] == "size")] <- NA
    }
    if (length(layout) == 2) {
      # Use two if statements as the next one needs layout to be length two
      # If a mixed layout with different aesthetics is used and
      if (all(c("col", "fill") %in% scale_order[["main_scales"]][["scales"]]) &
          # the default or just one colour scale is used (not a scale object since it will be aesthetic-specific)
          (all(sapply(col_scale, is.null)) |
           (identical(col_scale[[1]], col_scale[[2]]) & is.character(col_scale[[1]])))
      ) {
        # Hide one of the legends (col)
        scale_order[["main_scales"]][["order"]][which(scale_order[["main_scales"]][["scales"]] == "col")] <- NA
      }
    }
  }

  # Generate the necessary scales
  main_scales <- prepare_scales(scale_order = scale_order, context = "ggcorrhm",
                                layout = layout,
                                val_type = "continuous",
                                col_scale = col_scale, col_name = col_name,
                                size_scale = size_scale, size_name = size_name,
                                bins = bins, limits = limits,
                                high = high, mid = mid, low = low, midpoint = midpoint,
                                size_range = size_range, na_col = na_col)
  # Annotation scales
  annot_scales <- prepare_scales_annot(scale_order = scale_order, na_col = annot_na_col,
                                       annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df,
                                       annot_rows_col = annot_rows_col, annot_cols_col = annot_cols_col)

  # Generate the scale lists to pass to gghm
  col_scale <- extract_scales(main_scales, scale_order, c("fill", "col"), layout)
  size_scale <- extract_scales(main_scales, scale_order, "size", layout)

  # Call with all arguments to get the tooltips when calling ggcorrhm
  cor_plt <- gghm(cor_mat,
                  na_remove = na_remove,
                  mode = mode, layout = layout, include_diag = include_diag, return_data = TRUE,
                  col_scale = col_scale, col_name = col_name,
                  size_scale = size_scale, size_name = size_name,
                  border_col = border_col, border_lwd = border_lwd, border_lty = border_lty,
                  show_names_diag = show_names_diag, names_diag_params = names_diag_params,
                  show_names_x = show_names_x, names_x_side = names_x_side,
                  show_names_y = show_names_y, names_y_side = names_y_side,
                  cell_labels = cell_labels, cell_label_col = cell_label_col,
                  cell_label_size = cell_label_size, cell_label_digits = cell_label_digits,
                  cell_bg_col = cell_bg_col, cell_bg_alpha = cell_bg_alpha,
                  annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df,
                  annot_rows_col = annot_scales[["rows"]], annot_cols_col = annot_scales[["cols"]],
                  annot_rows_side = annot_rows_side, annot_cols_side = annot_cols_side,
                  annot_dist = annot_dist, annot_gap = annot_gap,
                  annot_size = annot_size, show_annot_names = show_annot_names,
                  annot_border_col = annot_border_col, annot_border_lwd = annot_border_lwd, annot_border_lty = annot_border_lty,
                  annot_na_col = annot_na_col, annot_na_remove = annot_na_remove,
                  annot_rows_params = annot_rows_params, annot_cols_params = annot_cols_params,
                  annot_names_size = annot_names_size,
                  annot_rows_names_side = annot_rows_names_side, annot_cols_names_side = annot_cols_names_side,
                  annot_rows_names_params = annot_rows_names_params, annot_cols_names_params = annot_cols_names_params,
                  annot_rows_name_params = annot_rows_name_params, annot_cols_name_params = annot_cols_name_params,
                  cluster_rows = cluster_rows, cluster_cols = cluster_cols,
                  cluster_distance = cluster_distance, cluster_method = cluster_method,
                  show_dend_rows = show_dend_rows, show_dend_cols = show_dend_cols,
                  dend_rows_side = dend_rows_side, dend_cols_side = dend_cols_side,
                  dend_col = dend_col, dend_dist = dend_dist, dend_height = dend_height, dend_lwd = dend_lwd, dend_lty = dend_lty,
                  dend_rows_params = dend_rows_params, dend_cols_params = dend_cols_params,
                  dend_rows_extend = dend_rows_extend, dend_cols_extend = dend_cols_extend,
                  split_rows = split_rows, split_cols = split_cols)

  if (return_data & any(unlist(p_values))) {
    # Add p-values to output data
    cor_plt[["plot_data"]] <- dplyr::left_join(cor_plt[["plot_data"]],
                                               dplyr::select(cor_mat_dat, "row", "col", "p_val", "p_adj"),
                                               by = c("row", "col"))
  }
  cor_out <- if (return_data) {cor_plt} else {cor_plt[["plot"]]}
  return(cor_out)
}


#' Prepare cell labels in ggcorrhm to pass to gghm
#'
#' @keywords internal
#'
#' @param mode Plotting mode.
#' @param cell_labels Cell labels (TRUE, FALSE, matrix, data frame).
#' @param p_values Logical indicating if p-values should be computed.
#' @param cell_label_p Logical indicating if cell labels should be swapped for p-values.
#' @param cell_label_digits Number of digits to display for cell labels.
#' @param p_thresholds P-value thresholds.
#' @param cor_mat_dat Correlation long format data from correlation tests.
#'
#' @returns Object to use as cell_labels in gghm (containing a logical, a matrix/data frame, or a list of length 2 with those things).
#'
prepare_cell_labels <- function(mode, cell_labels, p_values, cell_label_p, cell_label_digits,
                                p_thresholds = NULL, cor_mat_dat = NULL) {
  p_adj <- value <- NULL

  # If a matrix or data frame, put in a list
  if (is.matrix(cell_labels) | is.data.frame(cell_labels)) {
    cell_labels <- list(cell_labels)
  }
  # If just NULL is passed to mapply the result becomes list()
  # so put cell_label_digits in a list if not already, to allow for NULL to not round
  # (any other non-numeric also results in no rounding)
  if (!is.list(cell_label_digits)) {cell_label_digits <- list(cell_label_digits)}

  # Make p symbols if p-values are computed
  if (any(unlist(p_values))) {
    p_sym <- if (is.numeric(p_thresholds)) {
      # Make symbols for p-values
      as.character(symnum(x = cor_mat_dat[["p_adj"]],
                          # Add 0 to set the range of possible values
                          cutpoints = c(0, p_thresholds),
                          symbols = names(p_thresholds)))
    } else {
      # Nothing if no thresholds provided
      ""
    }
  }

  # Otherwise, iterate over values and check contents
  cell_labels <- mapply(function(md, lb, pv, lp, ld) {      # mode, labels, pvalues, label p, label digits
    # If matrix or data frame, make long format to replace correlation data
    if (is.matrix(lb) | is.data.frame(lb)) {
      lb_long <- shape_mat_long(lb)
      if (is.null(cor_mat_dat)) {
        cor_mat_dat <- lb_long
      } else {
        cor_mat_dat <- dplyr::left_join(dplyr::select(cor_mat_dat, -"value"), lb_long,
                                        by = c("row", "col"))
      }
    }

    # If cell_labels is not FALSE (TRUE or matrix/df) or using 'text' mode:
    # use as is if p_values and cell_label_p are FALSE
    # Otherwise, make matrix of values to display, taking p-values into consideration
    if (!isFALSE(lb) | md == "text") {
      if (!pv & !lp) {
        return(lb)

      } else if (!pv & lp) {
        # P-values are not computed so there is nothing display
        cli::cli_warn("{.var cell_label_p} is {.val TRUE} but {.var p_values} is {.val FALSE}.
                      Writing correlation values as no p-values have been computed.",
                      class = "cell_label_p_warn")
        return(lb)

      } else {
        # If p-values are computed, either add symbols to labels or swap labels for p-values

        cell_labels_long <- if (!lp) {
          cor_mat_dat
        } else {
          dplyr::mutate(cor_mat_dat, value = p_adj)
        }

        cell_labels_long[["value"]] <- if (is.numeric(cell_labels_long[["value"]]) & is.numeric(ld)) {
          round(cell_labels_long[["value"]], ld)
        } else {
          cell_labels_long[["value"]]
        }
        cell_labels_long <- dplyr::mutate(cell_labels_long,
                                          # Replace NAs with "" to not get "NA" written in the cells
                                          value = dplyr::case_when(is.na(value) ~ "", TRUE ~ as.character(value)),
                                          value = paste0(value, p_sym))
        cell_labels_wide <- shape_mat_wide(dplyr::select(cell_labels_long, row, col, value))

        return(cell_labels_wide)
      }

    } else if (isFALSE(lb)) {
      if (pv) {
        # No cell labels, but p-values are computed
        # Then display the p-value symbols
        cell_labels_long <- cor_mat_dat
        cell_labels_long[["value"]] <- p_sym
        cell_labels_wide <- shape_mat_wide(dplyr::select(cell_labels_long, row, col, value))

        return(cell_labels_wide)
      } else {
        # If just FALSE and no p-values, use as is
        return(lb)
      }
    }

  }, mode, cell_labels, p_values, cell_label_p, cell_label_digits, SIMPLIFY = FALSE)

  # If only one element, unlist it
  if (length(cell_labels) == 1) {cell_labels <- cell_labels[[1]]}

  return(cell_labels)
}

