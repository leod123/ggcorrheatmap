#' Make a heatmap with ggplot2.
#'
#' @param x Matrix or data frame in wide format to make a heatmap of. If rownames are present they are used for the y axis labels, otherwise the row number is used.
#' If a column named `.names` (containing unique row identifiers) is present it will be used as rownames.
#' @param layout String specifying the layout of the output heatmap. Possible layouts include
#' 'topleft', 'topright', 'bottomleft', 'bottomright', or the 'whole'/'full' heatmap (default and only possible option if the matrix is asymmetric).
#' A combination of the first letters of each word also works (i.e. f, w, tl, tr, bl, br).
#' If layout is of length two with two opposing triangles, a mixed layout will be used. For mixed layouts,
#' `mode` needs a vector of length two (applied in the same order as layout). See details for more information.
#' @param mode A string specifying plotting mode. Possible values are `heatmap`/`hm` for a normal heatmap, a number from 1 to 25 to draw the corresponding shape,
#' `text` to write the cell values instead of filling cells (colour scaling with value), and `none` for blank cells.
#' @param colr_scale Colour scale to use for cells. If NULL, the default ggplot2 scale is used. If a string, the corresponding Brewer or Viridis scales is used.
#' A string with a scale name with "rev_" in the beginning or "_rev" at the end will result in the reversed scale. Can also be a ggplot2 scale object.
#' @param colr_name String to use for the colour scale legend title. Can be two values in mixed layouts for dual scales.
#' @param limits Numeric vector of length two for the limits of the colour scale. NULL uses the default.
#' @param bins Number of bins to divide the scale into (if continuous values). A 'double' class value uses 'nice.breaks' to put the breaks at nice numbers which may not result
#' in the specified number of bins. If an integer the number of bins will be prioritised.
#' @param size_scale `ggplot2::scale_size_*` call to use for size scaling if `mode` is a number from 1 to 25 (R pch). In mixed layouts, can also be a list
#' of length two containing the two scales to use.
#' @param size_name String to use for the size scale legend title. Can be two values in mixed layouts for dual scales.
#' @param legend_order Integer vector specifying the order of legends (first value is for the first legend, second for the second, etc). The default (NULL) shows all legends.
#' NAs hide the corresponding legends, a single NA hides all. Ignored for `ggplot2` scale objects in `colr_scale` and `size_scale`.
#' @param include_diag Logical indicating if the diagonal cells (of a symmetric matrix) should be plotted.
#' Mostly only useful for getting a cleaner look with symmetric correlation matrices with triangular layouts, where the diagonal is known to be 1.
#' @param na_col Colour to use for cells with NA (both main heatmap and annotation).
#' @param na_remove Logical indicating if NA values in the heatmap should be omitted (meaning no cell border is drawn).
#' If NAs are kept, the fill colour can be set in the `ggplot2` scale.
#' @param return_data Logical indicating if the data used for plotting and clustering results should be returned.
#' @param cell_labels Logical specifying if the cells should be labelled with the values. Alternatively, a matrix or data frame with the same shape and dimnames as `x`
#' containing values to write in the cells. If mode is `text`, the cell label colours will scale with the cell values and `cell_label_col` is ignored.
#' @param cell_label_col Colour to use for cell labels, passed to `ggplot2::geom_text()`.
#' @param cell_label_size Size of cell labels, used as the `size` argument in `ggplot2::geom_text()`.
#' @param cell_label_digits Number of digits to display when cells are labelled (if numeric values). Default is 2, passed to `base::round()`. NULL for no rounding.
#' @param border_col Colour of cell borders. If `mode` is not a number, `border_col` can be set to NA to remove borders completely.
#' @param border_lwd Size of cell borders. If `mode` is a number, `border_col` can be set to 0 to remove borders.
#' @param border_lty Line type of cell borders. Not supported for numeric `mode`.
#' @param cell_bg_col Colour to use for cell backgrounds in modes 'text' and 'none'.
#' @param cell_bg_alpha Alpha for cell colours in modes 'text' and 'none'.
#' @param names_diag Logical indicating if names should be written in the diagonal cells (for symmetric input).
#' @param names_diag_params List with named parameters (such as size, angle, etc) passed on to geom_text when writing the column names in the diagonal.
#' @param names_x Logical indicating if names should be written on the x axis. Labels can be customised using `ggplot2::theme()` on the output plot.
#' @param names_x_side String specifying position of the x axis names ("top" or "bottom").
#' @param names_y Logical indicating if names should be written on the y axis.
#' @param names_y_side String specifying position of the y axis names ("left" or "right").
#' @param annot_rows_df Data frame for row annotations. The names of the columns in the data must be included,
#' either as row names or in a column named `.names`. Each other column specifies an annotation where the column name
#' will be used as the annotation name (in the legend and next to the annotation). Numeric columns will use a continuous
#' colour scale while factor or character columns use discrete scales.
#' @param annot_cols_df Same usage as `annot_rows_df` but for column annotation.
#' @param annot_rows_fill Named list for row annotation colour scales. The names should specify which annotation each scale applies to.
#' Elements can be strings or ggplot2 "Scale" class objects. If a string, it is used as the brewer palette or viridis option.
#' If a scale object it is used as is, allowing more flexibility. This may change the order that legends are drawn in,
#' specify order using the `guide` argument in the `ggplot2` scale function.
#' @param annot_cols_fill Named list used for column annotation colour scales, used like `annot_rows_fill`.
#' @param annot_rows_side String specifying which side row annotation should be drawn ('left' or 'l' for left, otherwise right).
#' @param annot_cols_side String specifying which side column annotation should be drawn ('bottom', 'down', 'b', 'd' for bottom, otherwise top).
#' @param annot_dist Distance between heatmap and first annotation cell where 1 is the size of one heatmap cell. Used for both row and column annotation.
#' @param annot_gap Distance between each annotation where 1 is the size of one heatmap cell. Used for both row and column annotation.
#' @param annot_size Size (width for row annotation, height for column annotation) of annotation cells compared to a heatmap cell. Used for both row and column annotation.
#' @param annot_label Logical controlling if names of annotations should be shown in the drawing area.
#' @param annot_border_col Colour of cell borders in annotation. By default it is the same as `border_col` of the main heatmap if it is of length 1, otherwise uses default (grey).
#' @param annot_border_lwd Line width of cell borders in annotation. By default it is the same as `border_lwd` of the main heatmap if it is of length 1, otherwise uses default (0.5).
#' @param annot_border_lty Line type of cell borders in annotation. By default it is the same as `border_lty` of the main heatmap if it is of length 1, otherwise uses default (solid).
#' @param annot_na_col Colour to use for NA values in annotations. Annotation-specific colour can be set in the ggplot2 scales in
#' the `annot_*_fill` arguments.
#' @param annot_na_remove Logical indicating if NAs in the annotations should be removed (producing empty spaces).
#' @param annot_rows_params Named list with parameters for row annotations to overwrite the defaults set by the `annot_*` arguments, each name corresponding to the `*` part
#' (see details for more information).
#' @param annot_cols_params Named list with parameters for column annotations, used like `annot_rows_params`.
#' @param annot_rows_label_side String specifying which side the row annotation labels should be on. Either "top" or "bottom".
#' @param annot_cols_label_side String specifying which side the column annotation labels should be on. Either "left" or "right".
#' @param annot_rows_label_params Named list of parameters for row annotation labels. Given to `grid::textGrob`, see `?grid::textGrob` for details. `?grid::gpar` is also helpful.
#' @param annot_cols_label_params Named list of parameters for column annotation labels. Given to `grid::textGrob`, see `?grid::textGrob` for details. `?grid::gpar` is also helpful.
#' @param cluster_rows Logical indicating if rows should be clustered. Can also be a `hclust` or `dendrogram` object.
#' @param cluster_cols Logical indicating if columns should be clustered. Can also be a `hclust` or `dendrogram` object.
#' @param cluster_distance String with the distance metric to use for clustering, given to `stats::dist()`.
#' @param cluster_method String with the clustering method to use, given to `stats::hclust()`.
#' @param dend_rows Logical indicating if a dendrogram should be drawn for the rows.
#' @param dend_cols Logical indicating if a dendrogram should be drawn for the columns.
#' @param dend_rows_side Which side to draw the row dendrogram on ('left' or 'l' for left, otherwise right).
#' @param dend_cols_side Which side to draw the column dendrogram on ('bottom', 'down', 'b', 'd' for bottom, otherwise top).
#' @param dend_col Colour to use for dendrogram lines, applied to both row and column dendrograms.
#' @param dend_dist Distance from heatmap (or annotation) to leaves of dendrogram, measured in heatmap cells (1 is the size of one cell).
#' @param dend_height Number by which to scale dendrogram height, applied to both row and column dendrograms.
#' @param dend_lwd Linewidth of dendrogram lines, applied to both row and column dendrograms.
#' @param dend_lty Dendrogram line type, applied to both row and column dendrograms.
#' @param dend_rows_params Named list for row dendrogram parameters to overwrite common parameter values. See details for more information.
#' @param dend_cols_params Named list for column dendrogram parameters to overwrite common parameter values. See details for more information.
#' @param dend_rows_extend Named list or functional sequence for specifying `dendextend` functions to apply to the row dendrogram. See details for usage.
#' @param dend_cols_extend Named list or functional sequence for specifying `dendextend` functions to apply to the column dendrogram. See details for usage.
#'
#' @return The heatmap as a `ggplot` object.
#' If `return_data` is TRUE the output is a list containing the plot (named 'plot'),
#' the plotting data ('plot_data', with factor columns 'row' and 'col' and a column 'value' containing the cell values),
#' and the result of the clustering ('row_clustering' and/or 'col_clustering).
#' If the layout is mixed, an extra factor column named 'layout' is included in 'plot_data', showing which triangle each cell belongs to.
#'
#' @export
#'
#' @details
#'
#' When using mixed layouts (`layout` is length two), `mode` needs to be length two as well, specifying the mode to use in each triangle.
#' The `cell_label_*` and `border_*` arguments can all be length one to apply to the whole heatmap, length two vectors to apply to each triangle,
#' or lists of length two, each element containing one value (apply to whole triangle) or a value per cell (apply cell-wise in triangle).
#' `cell_labels` can also be specified per triangle, either as a logical vector of length two, or a list of length two containing a mix of
#' logicals and matrices/data frames.
#'
#' It is also possible to provide two scales for filling or colouring the triangles differently.
#' In this case the `colr_scale` must be one character value (scale used for both triangles) or NULL or a list of length two
# containing the scales to use (character or scale object, or NULL for default). `size_scale` works in the same way (but takes no character values).
#'
#' The annotation parameter arguments `annot_rows_params` and `annot_cols_params` should be named lists, where the possible options correspond to
#' the different `annot_*` arguments. The possible options are "dist" (distance between heatmap and annotation), "gap" (distance between annotations),
#' "size" (cell size), "label" (logical, if the annotation names should be displayed), "border_col" (colour of border) and "border_lwd" (border line width).
#' Any unused options will use the defaults set by the `annot_*` arguments.
#'
#' The dendrogram parameters arguments `dend_rows_params` and `dend_cols_params` should be named lists, analogous to the annotation parameter arguments. Possible options are
#' "col" (line colour), "dist" (distance from heatmap to dendrogram), "height" (height scaling), "lwd" (line width), and "lty" (line type).
#'
#' The `dend_rows_extend` and `dend_cols_extend` arguments make it possible to customise the dendrograms using the `dendextend` package.
#' The argument should be a named list, each element named after the `dendextend` function to use (consecutive usage of the `set` function
#' is supported due to duplicate list names being possible). Each element should contain any arguments given to the `dendextend` function,
#' such as the `what` argument used in the `set` function. Alternatively, `dendextend` functions can be provided in a functional sequence ("fseq" object)
#' by piping together functions using the `%>%` pipe. Functions modifying the labels do not work as the dendrogram labels are not displayed (they are in the axis text).
#' As dendextend::as.ggdend() is used for conversion of the dendrogram, anything not supported by as.ggdend() will not work (such as "nodes_bg" or "rect.dendrogram").
#' See examples and the clustering article for example usage.
#'
#' @examples
#' library(ggplot2)
#'
#' # Use part of the mtcars data (for visibility)
#' hm_in <- mtcars[1:15, ]
#'
#' # Basic usage
#' gghm(hm_in)
#'
#' # Different layout (using a symmetric matrix)
#' gghm(cor(mtcars), layout = "tl")
#'
#' # Mixed layouts
#' gghm(cor(mtcars), layout = c("tr", "bl"),
#'      # Hide one of the legends
#'      legend_order = c(1, NA))
#'
#' # With clustering
#' gghm(scale(hm_in), cluster_rows = TRUE, cluster_cols = TRUE)
#'
#' # Adjusting cluster dendrograms using common and specific options
#' gghm(scale(hm_in), cluster_rows = TRUE, cluster_cols = TRUE,
#'      # Common options
#'      dend_lwd = 0.7, dend_col = "magenta",
#'      # Specific options
#'      dend_rows_params = list(height = 1), dend_cols_params = list(lty = 2))
#'
#' # With annotation and specifying colour scales
#' set.seed(123)
#' annot_rows <- data.frame(.names = rownames(hm_in),
#'                          annot1 = rnorm(nrow(hm_in)),
#'                          annot2 = sample(letters[1:3], nrow(hm_in), TRUE))
#' # Specify colour scale for one of the annotations (viridis mako)
#' annot_fill <- list(annot1 = "G")
#'
# gghm(scale(hm_in),
#      # Change colours of heatmap (Brewer Purples)
#      colr_scale = "Purples",
#      annot_rows_df = annot_rows, annot_rows_fill = annot_fill) +
#      # Use ggplot2::theme to adjust margins to fit the annotation labels
#      theme(plot.margin = margin(20, 10, 60, 20))
#'
#' # Using the dend_*_extend arguments
#' gghm(scale(hm_in), cluster_rows = TRUE, dend_rows_extend =
#'   list("set" = list("branches_lty", c(1, 2, 3)),
#'        # Empty list element (or NULL) if no arguments to be given
#'        "highlight_branches_col" = list()))
gghm <- function(x,
                 layout = "full",
                 mode = if (length(layout) == 1) "heatmap" else c("heatmap", "text"),
                 colr_scale = NULL, colr_name = "value", limits = NULL, bins = NULL,
                 size_scale = NULL, size_name = "value",
                 legend_order = NULL,
                 include_diag = TRUE, names_diag = FALSE, names_diag_params = NULL,
                 names_x = TRUE, names_x_side = "top", names_y = TRUE, names_y_side = "left",
                 na_col = "grey50", na_remove = FALSE, return_data = FALSE,
                 cell_labels = F, cell_label_col = "black", cell_label_size = 3, cell_label_digits = 2,
                 border_col = "grey", border_lwd = 0.1, border_lty = 1,
                 cell_bg_col = "white", cell_bg_alpha = 0,
                 annot_rows_df = NULL, annot_cols_df = NULL, annot_rows_fill = NULL, annot_cols_fill = NULL,
                 annot_rows_side = "right", annot_cols_side = "bottom",
                 annot_dist = 0.2, annot_gap = 0, annot_size = 0.5, annot_label = TRUE,
                 annot_border_col = if (length(border_col) == 1) border_col else "grey",
                 annot_border_lwd = if (length(border_lwd) == 1) border_lwd else 0.5,
                 annot_border_lty = if (length(border_lty) == 1) border_lty else 1,
                 annot_na_col = na_col, annot_na_remove = na_remove,
                 annot_rows_params = NULL, annot_cols_params = NULL,
                 annot_rows_label_side = "bottom", annot_cols_label_side = "left",
                 annot_rows_label_params = NULL, annot_cols_label_params = NULL,
                 cluster_rows = FALSE, cluster_cols = FALSE,
                 cluster_distance = "euclidean", cluster_method = "complete",
                 dend_rows = TRUE, dend_cols = TRUE, dend_rows_side = "right", dend_cols_side = "bottom",
                 dend_col = "black", dend_dist = 0, dend_height = 0.3, dend_lwd = 0.3, dend_lty = 1,
                 dend_rows_params = NULL, dend_cols_params = NULL,
                 dend_rows_extend = NULL, dend_cols_extend = NULL) {

  if (!is.matrix(x) & !is.data.frame(x)) cli::cli_abort("{.var x} must be a matrix or data frame.", class = "input_class_error")

  # Convert a tibble to a data frame to support row names
  if (inherits(x, "tbl_df")) {x <- as.data.frame(x)}

  # Check that there are colnames
  if (is.null(colnames(x))) {colnames(x) <- 1:ncol(x)}

  if (".names" %in% colnames(x)) {
    rownames(x) <- x[[".names"]]
    x <- dplyr::select(x, -".names")
  }
  # Explicitly define the rownames to prevent ggplot2 error if x is a data frame without explicit rownames
  rownames(x) <- rownames(x)

  # Check that there are rownames
  if (is.null(rownames(x))) {rownames(x) <- 1:nrow(x)}

  # Check if matrix becomes asymmetric after clustering to throw a warning
  x_sym <- isSymmetric(as.matrix(x))

  # Check layout and mode
  layout_check <- check_layout(layout, mode)

  # Check annotation data frames
  if (!is.null(annot_rows_df)) {
    annot_rows_df <- check_annot_df(annot_rows_df, rownames(x), "annot_rows_df")
  }
  if (!is.null(annot_cols_df)) {
    annot_cols_df <- check_annot_df(annot_cols_df, colnames(x), "annot_cols_df")
  }

  # Logical for full plot layout or not (mixed layout treated as full and triangular)
  full_plt <- if (length(layout) == 1) {layout %in% c("full", "f", "whole", "w")} else {TRUE}

  # If the matrix is asymmetric, triangular layouts break! Throw a warning
  if (!x_sym & (!full_plt | length(layout) == 2)) {
    cli::cli_warn("Triangular layouts are not supported for asymmetric matrices, plotting the full matrix instead.",
                  class = "force_full_warn")
    full_plt <- T
    layout <- "f"
    mode <- mode[1]
  }

  # Overwrite names_diag if the input is non-symmetric as it would cause
  # new ghost columns to be added to draw the names where row == col
  # This does not prevent diag names in initially symmetric matrices that become asymmetric as a result
  # of unequal clustering of rows and columns, the result will look a bit strange but no new columns are created
  if (!x_sym) {
    names_diag <- F
  }

  # If clustering a symmetric matrix with a triangular layout, both rows and columns must be clustered. Automatically cluster both and throw a warning
  if (x_sym & (!full_plt | length(layout) == 2)) {

    if (((!isFALSE(cluster_rows) & isFALSE(cluster_cols)) | (isFALSE(cluster_rows) & !isFALSE(cluster_cols)))) {
      cli::cli_warn("Cannot cluster only one dimension for triangular layouts, clustering both rows and columns instead.",
                    class = "force_clust_warn")
      if (isFALSE(cluster_rows)) {cluster_rows <- cluster_cols}
      else if (isFALSE(cluster_cols)) {cluster_cols <- cluster_rows}
    }
  }

  # Make dendrograms
  # To allow for providing a hclust or dendrogram object when clustering, make a separate logical clustering variable
  lclust_rows <- F
  if (!isFALSE(cluster_rows)) {
    row_clustering <- cluster_data(cluster_rows, x, cluster_distance, cluster_method, dend_rows_extend)

    # Reorder matrix to fit clustering
    x <- x[row_clustering$dendro$labels$label, ]
    lclust_rows <- T
  }

  lclust_cols <- F
  if (!isFALSE(cluster_cols)) {
    col_clustering <- cluster_data(cluster_cols, t(x), cluster_distance, cluster_method, dend_cols_extend)

    # Reorder matrix to fit clustering
    x <- x[, col_clustering$dendro$labels$label]
    lclust_cols <- T
  }

  # Throw a warning if clustering caused a symmetric input to become asymmetric. Plot the full matrix if not already the case
  if (x_sym & !isSymmetric(as.matrix(x))) {
    cli::cli_warn(paste0("The clustering has ordered rows and columns differently and caused the matrix to become asymmetric.",
                         ifelse(length(layout) == 2 | !full_plt, " Plotting the full matrix.", ""),
                         " The diagonal may be scrambled due to the unequal row and column orders."),
                  class = "unequal_clust_warn")
    layout <- "f"
    full_plt <- T
    mode <- mode[1]
  }

  # Evaluate annot_border_col, annot_border_lwd, annot_border_lty so they get values based on border_col etc
  # Otherwise they may be influenced by border_col etc becoming lists in the next step
  annot_border_col <- annot_border_col
  annot_border_lwd <- annot_border_lwd
  annot_border_lty <- annot_border_lty

  # Prepare parameters for mixed layouts
  if (length(layout) == 2) {
    # Allow for triangle-specific customisation in mixed layouts
    border_col <- prepare_mixed_param(border_col, "border_col")
    border_lwd <- prepare_mixed_param(border_lwd, "border_lwd")
    border_lty <- prepare_mixed_param(border_lty, "border_lty")
    cell_bg_col <- prepare_mixed_param(cell_bg_col, "cell_bg_col")
    cell_labels <- prepare_mixed_param(cell_labels, "cell_labels")
    cell_bg_alpha <- prepare_mixed_param(cell_bg_alpha, "cell_bg_alpha")
    cell_label_col <- prepare_mixed_param(cell_label_col, "cell_label_col")
    cell_label_size <- prepare_mixed_param(cell_label_size, "cell_label_size")
    cell_label_digits <- prepare_mixed_param(cell_label_digits, "cell_label_digits")
  }

  # Positions of different elements
  if (full_plt) {
    # Allow for specification of dendrogram positions only when the whole matrix is drawn
    dend_left <- dend_rows_side %in% c("left", "l")
    dend_down <- dend_cols_side %in% c("bottom", "down", "b", "d")
    annot_left <- annot_rows_side %in% c("left", "l")
    annot_down <- annot_cols_side %in% c("bottom", "down", "b", "d")
  } else {
    pos_left = dend_left = annot_left <- layout %in% c("topleft", "tl", "bottomleft", "bl")
    pos_down = dend_down = annot_down <- layout %in% c("bottomleft", "bl", "bottomright", "br")
  }

  # Make long format data, ordering columns to fit layout
  if (length(layout) == 1) {
    x_long <- layout_hm(x, layout = layout, na_remove = na_remove)

    # Reorder data to ensure it is in the drawing order
    x_long <- if (any(c("topleft", "tl", "bottomright", "br") %in% layout)) {
      dplyr::arrange(x_long, col, row)
    } else {
      dplyr::arrange(x_long, col, dplyr::desc(row))
    }

  } else if (length(layout) == 2) {
    # Mixed layout, generate one per half and mark by layout. The first one gets the diagonal
    x_long <- dplyr::bind_rows(
      dplyr::mutate(layout_hm(x, layout = layout[1], na_remove = na_remove, include_diag = T), layout = layout[1]),
      dplyr::mutate(layout_hm(x, layout = layout[2], na_remove = na_remove, include_diag = F), layout = layout[2])
    )
    # Convert layout to a factor vector
    x_long[["layout"]] <- factor(x_long[["layout"]], levels = layout)

    x_long <- if (any(c("topleft", "tl", "bottomright", "br") %in% layout)) {
      dplyr::arrange(x_long, layout, col, row)
    } else {
      dplyr::arrange(x_long, layout, col, dplyr::desc(row))
    }
  }

  # Generate colour scales according to specifications
  # Skip this whole part if the function call comes directly from ggcorrhm, as it is already handled there
  if (!grepl("^ggcorrhm\\(", deparse(sys.call(-1))[1])) {
    # Get scales and their orders
    scale_order <- make_legend_order(mode = mode,
                                     colr_scale = colr_scale,
                                     size_scale = size_scale, annot_rows_df = annot_rows_df,
                                     annot_cols_df = annot_cols_df, legend_order = legend_order)

    # Prepare scales for mixed layouts
    if (length(layout) == 2) {
      colr_name <- prepare_mixed_param(colr_name, "colr_name")
      colr_scale <- prepare_mixed_param(colr_scale, "colr_scale")
      size_name <- prepare_mixed_param(size_name, "size_name")
      size_scale <- prepare_mixed_param(size_scale, "size_scale")
    }

    # Generate the necessary scales
    main_scales <- prepare_scales(scale_order = scale_order, context = "gghm",
                                  val_type = ifelse(is.character(x_long[["value"]]) | is.factor(x_long[["value"]]), "discrete", "continuous"),
                                  colr_scale = colr_scale, colr_name = colr_name,
                                  size_scale = size_scale, size_name = size_name,
                                  na_col = na_col, limits = limits, bins = bins)
    # Annotation scales
    annot_scales <- prepare_scales_annot(scale_order = scale_order, na_col = annot_na_col,
                                         annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df,
                                         annot_rows_col = annot_rows_fill, annot_cols_col = annot_cols_fill)

    # Generate the scale lists to pass to gghm
    colr_scale <- extract_scales(main_scales, scale_order, c("fill", "col"), layout)
    size_scale <- extract_scales(main_scales, scale_order, "size", layout)
  } else {
    annot_scales <- list("rows" = annot_rows_fill, "cols" = annot_cols_fill)
  }

  # Annotation for rows and columns
  # Default annotation parameters
  annot_default <- list(dist = annot_dist, gap = annot_gap, size = annot_size,
                        label = annot_label, border_col = annot_border_col, border_lwd = annot_border_lwd,
                        border_lty = annot_border_lty, na_col = annot_na_col)
  if (is.data.frame(annot_rows_df)) {
    annot_rows_prep <- prepare_annotation(annot_df = annot_rows_df, annot_defaults = annot_default,
                                          annot_params = annot_rows_params, lannot_side = annot_left,
                                          annot_label_params = annot_rows_label_params,
                                          annot_label_side = annot_rows_label_side, data_size = ncol(x))
    annot_rows_df <- annot_rows_prep[[1]]; annot_rows_params <- annot_rows_prep[[2]];
    annot_rows_pos <- annot_rows_prep[[3]]; annot_rows_label_params <- annot_rows_prep[[4]]
  }

  if (is.data.frame(annot_cols_df)) {
    annot_cols_prep <- prepare_annotation(annot_df = annot_cols_df, annot_defaults = annot_default,
                                          annot_params = annot_cols_params, lannot_side = annot_down,
                                          annot_label_params = annot_cols_label_params,
                                          annot_label_side = annot_cols_label_side, data_size = nrow(x))
    annot_cols_df <- annot_cols_prep[[1]]; annot_cols_params <- annot_cols_prep[[2]];
    annot_cols_pos <- annot_cols_prep[[3]]; annot_cols_label_params <- annot_cols_prep[[4]]
  }

  # Generate dendrograms, positions depend on annotation sizes
  dend_defaults <- list(dist = dend_dist, col = dend_col, height = dend_height, lwd = dend_lwd, lty = dend_lty)
  if (lclust_rows & dend_rows) {
    # Replace default parameters if any are provided
    dend_rows_params <- replace_default(dend_defaults, dend_rows_params)

    dendro_rows <- prepare_dendrogram(dendro_in = row_clustering$dendro, dend_dim = "rows",
                                      dend_down = dend_down, dend_left = dend_left,
                                      dend_dist = dend_rows_params$dist,
                                      dend_height = dend_rows_params$height,
                                      full_plt = full_plt, layout = layout, x_long = x_long,
                                      annot_df = annot_rows_df, annot_side = annot_left,
                                      annot_pos = annot_rows_pos, annot_size = annot_rows_params$size)
    # Check that the dendrogram labels are in the correct positions after mirroring and shifting
    dendro_rows <- check_dendrogram_pos(dat = x_long, dend_dim = "row", dendro = dendro_rows)
  }

  if (lclust_cols & dend_cols) {
    dend_cols_params <- replace_default(dend_defaults, dend_cols_params)

    dendro_cols <- prepare_dendrogram(dendro_in = col_clustering$dendro, dend_dim = "cols",
                                      dend_down = dend_down, dend_left = dend_left,
                                      dend_dist = dend_cols_params$dist,
                                      dend_height = dend_cols_params$height,
                                      full_plt = full_plt, layout = layout, x_long = x_long,
                                      annot_df = annot_cols_df, annot_side = annot_down,
                                      annot_pos = annot_cols_pos, annot_size = annot_cols_params$size)
    dendro_cols <- check_dendrogram_pos(x_long, "col", dendro_cols)
  }

  # Build plot
  if (length(layout) == 1) {
    plt <- make_heatmap(x_long = x_long, plt = NULL, mode = mode, include_diag = include_diag,
                        invisible_diag = isSymmetric(as.matrix(x)) & !include_diag,
                        border_lwd = border_lwd, border_col = border_col, border_lty = border_lty,
                        names_diag = names_diag, names_x = names_x, names_y = names_y,
                        names_x_side = names_x_side, names_y_side = names_y_side,
                        colr_scale = colr_scale, size_scale = size_scale,
                        cell_labels = cell_labels, cell_label_col = cell_label_col,
                        cell_label_size = cell_label_size, cell_label_digits = cell_label_digits,
                        cell_bg_col = cell_bg_col, cell_bg_alpha = cell_bg_alpha)
    if (names_diag) {
      plt <- add_diag_names(plt = plt, x_long = x_long, names_diag_params = names_diag_params)
    }
  } else if (length(layout) == 2) {

    # Avoid name clash
    lt <- layout
    # First half of the plot
    plt <- make_heatmap(x_long = dplyr::filter(x_long, layout == lt[1]), plt = NULL,
                        mode = mode[1], include_diag = include_diag, invisible_diag = T,
                        border_lwd = border_lwd[[1]], border_col = border_col[[1]], border_lty = border_lty[[1]],
                        names_diag = names_diag, names_x = names_x, names_y = names_y,
                        names_x_side = names_x_side, names_y_side = names_y_side,
                        colr_scale = colr_scale[[1]], size_scale = size_scale[[1]],
                        cell_labels = cell_labels[[1]], cell_label_col = cell_label_col[[1]],
                        cell_label_size = cell_label_size[[1]], cell_label_digits = cell_label_digits[[1]],
                        cell_bg_col = cell_bg_col[[1]], cell_bg_alpha = cell_bg_alpha[[1]])
    # Remaining half
    # Add new scales if multiple are provided
    if (isTRUE(colr_scale[[1]][["aesthetics"]] == colr_scale[[2]][["aesthetics"]])) {
      plt <- plt + ggnewscale::new_scale(colr_scale[[1]][["aesthetics"]])
    }
    if (!is.null(size_scale[[2]])) {plt <- plt + ggnewscale::new_scale(new_aes = "size")}

    plt <- make_heatmap(x_long = dplyr::filter(x_long, layout == lt[2]), plt = plt,
                        mode = mode[2], include_diag = F, invisible_diag = F,
                        border_lwd = border_lwd[[2]], border_col = border_col[[2]], border_lty = border_lty[[2]],
                        names_diag = F, names_x = names_x, names_y = names_y,
                        names_x_side = names_x_side, names_y_side = names_y_side,
                        colr_scale = colr_scale[[2]], size_scale = size_scale[[2]],
                        cell_labels = cell_labels[[2]], cell_label_col = cell_label_col[[2]],
                        cell_label_size = cell_label_size[[2]], cell_label_digits = cell_label_digits[[2]],
                        cell_bg_col = cell_bg_col[[2]], cell_bg_alpha = cell_bg_alpha[[2]])
    if (names_diag) {
      plt <- add_diag_names(plt = plt, x_long = x_long, names_diag_params = names_diag_params)
    }
  }

  # Add row and column annotations
  if (is.data.frame(annot_rows_df)) {
    # Change legend order to be in order of annotations (after main values)
    lgd_order <- seq_along(annot_rows_df)[-1] + 1
    names(lgd_order) <- colnames(annot_rows_df)[-which(colnames(annot_rows_df) == ".names")]

    plt <- add_annotation(plt = plt, annot_dim = "rows", annot_df = annot_rows_df, annot_pos = annot_rows_pos,
                          annot_size = annot_rows_params$size, annot_border_lwd = annot_rows_params$border_lwd,
                          annot_border_col = annot_rows_params$border_col, annot_border_lty = annot_rows_params$border_lty,
                          draw_legend = annot_rows_params$legend, draw_label = annot_rows_params$label,
                          na_remove = annot_na_remove, col_scale = annot_scales[["rows"]],
                          legend_order = lgd_order, label_side = annot_rows_label_side, label_params = annot_rows_label_params)
  }

  if (is.data.frame(annot_cols_df)) {
    lgd_order <- if (is.data.frame(annot_rows_df)) {seq_along(annot_cols_df)[-1] + ncol(annot_rows_df)}
    else {seq_along(annot_cols_df)[-1] + 1}
    names(lgd_order) <- colnames(annot_cols_df)[-which(colnames(annot_cols_df) == ".names")]

    plt <- add_annotation(plt = plt, annot_dim = "cols", annot_df = annot_cols_df, annot_pos = annot_cols_pos,
                          annot_size = annot_cols_params$size, annot_border_lwd = annot_cols_params$border_lwd,
                          annot_border_col = annot_cols_params$border_col, annot_border_lty = annot_cols_params$border_lty,
                          draw_legend = annot_cols_params$legend, draw_label = annot_cols_params$label,
                          na_remove = annot_na_remove, col_scale = annot_scales[["cols"]],
                          legend_order = lgd_order, label_side = annot_cols_label_side, label_params = annot_cols_label_params)
  }

  # Add dendrograms
  if (lclust_rows & dend_rows) {
    plt <- add_dendrogram(plt = plt, dendro = dendro_rows, dend_col = dend_rows_params$col,
                          dend_lwd = dend_rows_params$lwd, dend_lty = dend_rows_params$lty)
  }

  if (lclust_cols & dend_cols) {
    plt <- add_dendrogram(plt = plt, dendro = dendro_cols, dend_col = dend_cols_params$col,
                          dend_lwd = dend_cols_params$lwd, dend_lty = dend_cols_params$lty)
  }

  if (return_data) {

    # If layout is included (for mixed layouts), rename column
    if ("lt" %in% colnames(x_long)) {x_long <- dplyr::rename(x_long, layout = lt)}

    # Data to return. If not drawing the diagonal, skip those values
    data_out <- if (!include_diag) {
      dplyr::filter(x_long, as.character(row) != as.character(col))
    } else {
      x_long
    }
    list_out <- list("plot" = plt, "plot_data" = data_out)

    if (!isFALSE(cluster_rows)) {
      list_out[["row_clustering"]] <- row_clustering[["clust"]]
    }
    if (!isFALSE(cluster_cols)) {
      list_out[["col_clustering"]] <- col_clustering[["clust"]]
    }

    return(list_out)

  } else {
    return(plt)
  }
}


#' Check that layout and mode are correct
#'
#' @keywords internal
#'
#' @param layout Plot layout.
#' @param mode Plot mode.
#'
#' @returns Error if incorrect layout or mode, otherwise nothing.
#'
check_layout <- function(layout, mode) {
  # Check layout and mode
  supported_layouts <- c("full", "f", "whole", "w",
                         "bottomleft", "bl", "topleft", "tl",
                         "topright", "tr", "bottomright", "br")
  supported_modes <- c("heatmap", "hm", "text", as.character(1:25), "none")
  mode_cli_vec <- cli::cli_vec(supported_modes, list("vec-trunc" = 6)) # For error messages

  if (!(length(mode) == 1 & length(layout) == 1) & !(length(mode) == 2 & length(layout) == 2)) {
    cli::cli_abort("{.var layout} and {.var mode} must be the same length (1 or 2).", class = "layout_mode_len_error")
  }

  if (length(layout) == 1) {
    if (!layout %in% supported_layouts) {
      cli::cli_abort(c("{.val {layout}} is not a supported layout.",
                       "i" = "Supported layouts are {.val {supported_layouts}}"),
                     class = "nonsup_layout_error")
    }

    if (!mode %in% supported_modes) {
      cli::cli_abort(c("{.val {mode}} is not a supported mode for this layout.",
                       "i" = paste0("When {.var layout} has length 1, {.var mode} must be ", cli::style_bold("one"), " of {.val {mode_cli_vec}}")),
                     class = "nonsup_mode_error")
    }

  } else if (length(layout) == 2) {
    # Only allow for combinations of topleft + bottomright and topright + bottomleft
    if (!((sum(layout %in% c("topleft", "tl")) == 1 & sum(layout %in% c("bottomright", "br")) == 1) |
          (sum(layout %in% c("topright", "tr")) == 1 & sum(layout %in% c("bottomleft", "bl")) == 1))) {
      cli::cli_abort(c("{.val {layout}} is not a supported combination of layouts.",
                       "i" = "Mixed layouts must consist of combinations of opposite triangles."),
                     class = "nonsup_layout_error")
    }
    # mode must also be length 2
    if (any(!mode %in% supported_modes)) {
      cli::cli_abort(c("{.val {mode}} is not a supported combination of modes.",
                       "i" = paste0("When {.var layout} has length 2, {.var mode} must be ", cli::style_bold("two"), " of {.val {mode_cli_vec}}")),
                     class = "nonsup_mode_error")
    }

  }
}

#' Prepare parameters for mixed layouts.
#'
#' @keywords internal
#'
#' @param param Parameter to prepare.
#' @param param_name Parameter name (for error messages and handling special parameter).
#'
#' @returns If length one, it is returned duplicated in a list for use in each triangle. If longer than 1
#' an error message is returned. In other cases, the input parameter is returned (length two input).
#'
prepare_mixed_param <- function(param, param_name) {

  if (grepl("_scale$", param_name) && is.list(param) && length(param) == 1) {
    # If a scale object, put it in a list but don't repeat it
    # If character, repeat
    if (inherits(param[[1]], c("Scale", "ggproto", "gg"))) {
      param_out <- list(param[[1]], NULL)
    } else if (is.character(param[[1]])) {
      param_out <- list(param[[1]], param[[1]])
    }

  } else if (is.null(param) && (grepl("_scale$", param_name) || grepl("_digits$", param_name))) {
    # Return a list of NULLs if NULL
    param_out <- list(NULL, NULL)

  } else if (length(param) == 1 || (param_name == "cell_labels" && (is.matrix(param) || is.data.frame(param)))) {
    # Recycle if length one, or if cell_labels is a data frame or matrix for cell labels
    param_out <- list(param, param)

  } else if (length(param) != 2) {
    msg <- if (param_name == "colr_scale") {
      "In a mixed layout, {.var {param_name}} must be either one character specifying a scale (used for all),
       or a list with up to two scales (character, scale object or NULL). See the details of 'gghm' for more on usage."
    } else if (param_name == "size_scale") {
      "In a mixed layout, {.var {param_name}} must be either NULL (use default),
       or a list with up to two scales (scale object or NULL). See the details of 'gghm' for more on usage."
    } else {
      "In a mixed layout, {.var {param_name}} must be either one (used for all) or two (used for each triangle) values,
                          or a list of length two with a vector for each triangle. See the details of 'gghm' for more on usage."
    }

    cli::cli_abort(msg, class = "param_len_error")

  } else {
    param_out <- param
  }

  return(param_out)
}
