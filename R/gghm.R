#' Make a heatmap with ggplot2.
#'
#' @param x Matrix or data frame in wide format to make a heatmap of. If rownames are present they are used for the y axis labels, otherwise the row number is used.
#' If a column named `.names` (containing unique row identifiers) is present it will be used as rownames.
#' @param fill_scale A `ggplot2` scale object for the cell fill colour scale. NULL (default) uses the `ggplot2` default.
#' @param fill_name String to use for the fill scale legend title.
#' @param col_scale A `ggplot2` scale object for colour scale (Applied to text and non-filled shapes). NULL (default) uses the `ggplot2` default.
#' @param col_name String to use for the colour scale legend title.
#' @param mode A string specifying plotting mode. Possible values are `heatmap`/`hm` for a normal heatmap, a number from 1 to 25 to draw the corresponding shape,
#' `text` to write the cell values instead of cells (colour scaling with value), and `none` for blank cells.
#' @param layout String specifying the layout of the output heatmap. Possible layouts include
#' 'topleft', 'topright', 'bottomleft', 'bottomright', or the 'whole'/'full' heatmap (default and only possible option if the matrix is asymmetric).
#' A combination of the first letters of each word also works (i.e. f, w, tl, tr, bl, br).
#' If layout is of length two with two opposing triangles, a mixed layout will be used. For mixed layouts,
#' `mode` needs a vector of length two (applied in the same order as layout). See details for more information.
#' @param include_diag Logical indicating if the diagonal cells (of a symmetric matrix) should be plotted.
#' Mostly only useful for getting a cleaner look with symmetric correlation matrices with triangular layouts, where the diagonal is known to be 1.
#' @param na_remove Logical indicating if NA values in the heatmap should be omitted (meaning no cell border is drawn).
#' If NAs are kept, the fill colour can be set in the `ggplot2` scale.
#' @param return_data Logical indicating if the data used for plotting and clustering results should be returned.
#' @param show_legend Logical vector indicating if main heatmap legends (fill, colour and size) should be shown. If length 1 it is applied to all legends.
#' Can be specified in an aesthetic-specific manner using a named vector like `c('fill' = TRUE, 'size' = FALSE)`.
#' @param size_scale `ggplot2::scale_size_*` call to use for size scaling if `mode` is a number from 1 to 25 (R pch).
#' @param cell_labels Logical specifying if the cells should be labelled with the values.
#' @param cell_label_col Colour to use for cell labels, passed to `ggplot2::geom_text`.
#' @param cell_label_size Size of cell labels, used as the `size` argument in `ggplot2::geom_text`.
#' @param cell_label_digits Number of digits to display when cells are labelled (if numeric values). Default is 2, passed to `round`. NULL for no rounding.
#' @param border_col Colour of cell borders. If `mode` is not a number, `border_col` can be set to NA to remove borders completely.
#' @param border_lwd Size of cell borders. If `mode` is a number, `border_col` can be set to 0 to remove borders.
#' @param border_lty Line type of cell borders. Not supported for numeric `mode`.
#' @param names_diag Logical indicating if names should be written in the diagonal cells (for a symmetric matrix).
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
#' @param annot_rows_fill Named list for row annotation colour scales. The names should specify which annotation each scale applies to.
#' Elements can be strings or ggplot2 "Scale" class objects. If a string, it is used as the brewer palette (categorical variables) or viridis option (continuous variables).
#' If a scale object it is used as is, allowing more flexibility. This may change the order that legends are drawn in,
#' specify order using the `guide` argument in the `ggplot2` scale function.
#' @param annot_cols_fill Named list used for column annotation colour scales, used like `annot_rows_fill`.
#' @param annot_rows_side String specifying which side row annotation should be drawn ('left' or 'l' for left, otherwise right).
#' @param annot_cols_side String specifying which side column annotation should be drawn ('bottom', 'down', 'b', 'd' for bottom, otherwise top).
#' @param annot_legend Logical indicating if row and column annotations should have legends.
#' @param annot_dist Distance between heatmap and first annotation cell where 1 is the size of one heatmap cell. Used for both row and column annotation.
#' @param annot_gap Distance between each annotation where 1 is the size of one heatmap cell. Used for both row and column annotation.
#' @param annot_size Size (width for row annotation, height for column annotation) of annotation cells. Used for both row and column annotation.
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
#' @param cluster_rows Logical indicating if rows should be clustered.
#' @param cluster_cols Logical indicating if columns should be clustered.
#' @param cluster_distance String with the distance metric to use for clustering, given to `dist`.
#' @param cluster_method String with the clustering method to use, given to `hclust`.
#' @param dend_rows Logical indicating if a dendrogram should be drawn for the rows.
#' @param dend_cols Logical indicating if a dendrogram should be drawn for the columns.
#' @param dend_rows_side Which side to draw the row dendrogram on ('left' or 'l' for left, otherwise right).
#' @param dend_cols_side Which side to draw the column dendrogram on ('bottom', 'down', 'b', 'd' for bottom, otherwise top).
#' @param dend_col Colour to use for dendrogram lines, applied to both row and column dendrograms.
#' @param dend_height Number by which to scale dendrogram height, applied to both row and column dendrograms.
#' @param dend_lwd Linewidth of dendrogram lines, applied to both row and column dendrograms.
#' @param dend_lty Dendrogram line type, applied to both row and column dendrograms.
#' @param dend_rows_params Named list for row dendrogram parameters. See details for more information.
#' @param dend_cols_params Named list for column dendrogram parameters. See details for more information.
#' @param dend_rows_extend Named list or functional sequence for specifying `dendextend` functions to apply to the row dendrogram. See details for usage.
#' @param dend_cols_extend Named list or functional sequence for specifying `dendextend` functions to apply to the column dendrogram. See details for usage.
#'
#' @return The heatmap as a `ggplot` object.
#' If `return_data` is TRUE the output is a list containing the plot (named 'plot'),
#' the plotting data ('plot_data'), and the result of the clustering ('row_clustering' and/or 'col_clustering).
#' If the layout is mixed, an extra column named 'layout' is included, showing which triangle each cell belongs to.
#'
#' @export
#'
#' @details
#'
#' When using mixed layouts (`layout` is length two), `mode` needs to be length two as well, specifying the mode to use in each triangle.
#' The `cell_label_*` and `border_*` arguments can all be length one to apply to the whole heatmap, length two vectors to apply to each triangle,
#' or lists of length two, each element containing one value (apply to whole triangle) or a value per cell (apply cell-wise in triangle). (`cell_labels` can also be a vector of length two).
#'
#' The annotation parameter arguments `annot_rows_params` and `annot_cols_params` should be named lists, where the possible options correspond to
#' the different `annot_*` arguments. The possible options are "legend" (logical, if legends should be drawn), "dist" (distance between heatmap and annotation), "gap" (distance between annotations),
#' "size" (cell size), "label" (logical, if the annotation names should be displayed), "border_col" (colour of border) and "border_lwd" (border line width).
#' Any unused options will use the defaults set by the `annot_*` arguments.
#'
#' The dendrogram parameters arguments `dend_rows_params` and `dend_cols_params` should be named lists, analogous to the annotation parameter arguments. Possible options are
#' "col" (line colour), "height" (height scaling), "lwd" (line width), and "lty" (line type).
#'
#' The `dend_rows_extend` and `dend_cols_extend` arguments make it possible to customise the dendrograms using the `dendextend` package.
#' The argument should be a named list, each element named after the `dendextend` function to use (consecutive usage of the `set` function
#' is supported due to duplicate list names being possible). Each element should contain any arguments given to the `dendextend` function,
#' such as the `what` argument used in the `set` function. Alternatively, `dendextend` functions can be provided in a functional sequence ("fseq" object)
#' by piping together functions using the `%>%` pipe. See examples and the clustering vignette for example usage.
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
#'   show_legend = c(fill = TRUE, colour = FALSE))
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
#' gghm(scale(hm_in),
#'      # Change colours of heatmap
#'      fill_scale = ggplot2::scale_fill_gradient(low = "beige", high = "sienna2"),
#'      annot_rows_df = annot_rows, annot_rows_fill = annot_fill) +
#'      # Use ggplot2::theme to adjust margins to fit the annotation labels
#'      theme(plot.margin = margin(20, 10, 60, 20))
#'
#' # Using the dend_*_extend arguments
#' gghm(scale(hm_in), cluster_rows = TRUE, dend_rows_extend =
#'   list("set" = list("branches_lty", c(1, 2, 3)),
#'        # Empty list element (or NULL) if no arguments to be given
#'        "highlight_branches_col" = list()))
gghm <- function(x, fill_scale = NULL, fill_name = "value", col_scale = NULL, col_name = fill_name,
                 mode = if (length(layout) == 1) "heatmap" else c("heatmap", "text"),
                 layout = "full", include_diag = TRUE, na_remove = FALSE, return_data = FALSE,
                 show_legend = c("fill" = TRUE, "colour" = TRUE, "size" = TRUE), size_scale = NULL,
                 cell_labels = F, cell_label_col = "black", cell_label_size = 3, cell_label_digits = 2,
                 border_col = "grey", border_lwd = 0.5, border_lty = 1,
                 names_diag = FALSE, names_diag_param = NULL,
                 names_x = TRUE, names_x_side = "top", names_y = TRUE, names_y_side = "left",
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

  if (!is.matrix(x) & !is.data.frame(x)) stop("x must be a matrix or data frame.")

  # Convert a tibble to a data frame to support row names
  if (inherits(x, "tbl_df")) {x <- as.data.frame(x)}

  # Check that there are colnames
  if (is.null(colnames(x))) {colnames(x) <- 1:ncol(x)}

  if (".names" %in% colnames(x)) {
    rownames(x) <- x[[".names"]]
    x <- dplyr::select(x, -.names)
  }
  # Explicitly define the rownames to prevent ggplot2 error if x is a data frame without explicit rownames
  rownames(x) <- rownames(x)

  # Check that there are rownames
  if (is.null(rownames(x))) {rownames(x) <- 1:nrow(x)}

  x_mat <- as.matrix(x)

  # Logical for full plot layout or not
  full_plt <- if (length(layout) == 1) {layout %in% c("full", "f", "whole", "w")} else {TRUE}

  # If the matrix is asymmetric, triangular layouts break! Throw a warning
  if (!isSymmetric(x_mat) & (!full_plt | length(layout) == 2)) {
    warning("Triangular layouts are not supported for asymmetric matrices,\nplotting the full matrix instead.")
    full_plt <- T
    layout <- "f"
    mode <- mode[1]
  }

  # Check that layout is valid
  if (length(layout) == 1) {
    if (!layout %in% c("full", "f", "whole", "w",
                       "bottomleft", "bl", "topleft", "tl",
                       "topright", "tr", "bottomright", "br")) {
      stop("Not a supported layout. Supported layouts are
         full/f/whole/w,
         topleft/tl, topright/tr,
         bottomleft/bl, and bottomright/br")
    }

    # Check that mode is also ok
    if (!mode %in% c("heatmap", "hm", as.character(1:25), "text", "none")) {
      stop("mode must be one of 'heatmap' ('hm'), '1'-'25', 'text', or 'none'.")
    }

  } else if (length(layout) == 2) {
    # Only allow for combinations of topleft + bottomright and topright + bottomleft
    if (!((sum(layout %in% c("topleft", "tl")) == 1 & sum(layout %in% c("bottomright", "br")) == 1) |
          (sum(layout %in% c("topright", "tr")) == 1 & sum(layout %in% c("bottomleft", "bl")) == 1))) {
      stop("Mixed layouts must consist of combinations of opposite triangles.")
    }

    # border_* and cell_label*  allow for triangle-specific customisation in mixed layouts
    border_col <- prepare_mixed_param(border_col, "border_col")
    border_lwd <- prepare_mixed_param(border_lwd, "border_lwd")
    border_lty <- prepare_mixed_param(border_lty, "border_lty")
    cell_labels <- prepare_mixed_param(cell_labels, "cell_labels")
    cell_label_col <- prepare_mixed_param(cell_label_col, "cell_label_col")
    cell_label_size <- prepare_mixed_param(cell_label_size, "cell_label_size")
    cell_label_digits <- prepare_mixed_param(cell_label_digits, "cell_label_digits")

    # mode must also be length 2
    if (length(mode) != 2 | any(!mode %in% c("heatmap", "hm", as.character(1:25), "text", "none"))) {
      stop("mode must be two of 'heatmap' ('hm'), '1'-'25', 'text', and 'none'.")
    }
  } else {
    stop("layout must be length 1 or 2.")
  }

  # Don't display names on the diagonal if the plot is non-symmetric as it will cause
  # new ghost columns to be added to draw the names where row == col
  if (!isSymmetric(x_mat)) {
    names_diag <- F
  }

  # If clustering a symmetric matrix with a triangular layout, both rows and columns must be clustered. Automatically cluster both and throw a warning
  if (isSymmetric(x_mat) & (!full_plt | length(layout) == 2)) {

    if (((!isFALSE(cluster_rows) & isFALSE(cluster_cols)) | (isFALSE(cluster_rows) & !isFALSE(cluster_cols)))) {
      warning("Cannot cluster only one dimension for triangular layouts, clustering both rows and columns.")
      if (isFALSE(cluster_rows)) {cluster_rows <- cluster_cols}
      else if (isFALSE(cluster_cols)) {cluster_cols <- cluster_rows}
    }

    # If different clusterings are provided, warn that the output may look strange
    if ((inherits(cluster_rows, "hclust") | inherits(cluster_cols, "hclust")) &
        !identical(cluster_rows, cluster_cols)) {
      warning("If the row and column clusterings are not identical the output may look strange with triangular layouts.")
    }
  }

  # Make dendrograms
  # To allow for providing a hclust object when clustering, make a separate logical clustering variable
  lclust_rows <- F
  if (!isFALSE(cluster_rows)) {
    row_clustering <- cluster_dimension(cluster_rows, x_mat, cluster_distance, cluster_method, dend_rows_extend)

    # Reorder matrix to fit clustering
    x_mat <- x_mat[row_clustering$dendro$labels$label, ]
    lclust_rows <- T
  }

  lclust_cols <- F
  if (!isFALSE(cluster_cols)) {
    col_clustering <- cluster_dimension(cluster_cols, t(x_mat), cluster_distance, cluster_method, dend_cols_extend)

    # Reorder matrix to fit clustering
    x_mat <- x_mat[, col_clustering$dendro$labels$label]
    lclust_cols <- T
  }

  # Make long format data, ordering columns to fit layout
  if (length(layout) == 1) {
    x_long <- layout_hm(x_mat, layout = layout, na_remove = na_remove)

  } else if (length(layout) == 2) {
    # Mixed layout, generate one per half and mark by layout. The first one gets the diagonal
    x_long <- dplyr::bind_rows(
      dplyr::mutate(layout_hm(x_mat, layout = layout[1], na_remove = na_remove, include_diag = T), lt = layout[1]),
      dplyr::mutate(layout_hm(x_mat, layout = layout[2], na_remove = na_remove, include_diag = F), lt = layout[2])
    )
  }

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

  # Set colour scale if none provided to change order of legends (if not set, the legend may end up after the annotation legens)
  # Check if the fill legend is supposed to be drawn at all (otherwise it might draw the legend even if show_legend is c(fill = FALSE))
  show_fill <- if ("fill" %in% names(show_legend)) show_legend["fill"] else show_legend
  # Different depending on class of input data
  if (is.null(fill_scale)) {
    fill_scale <- if (is.character(x_long$value) | is.factor(x_long$value)) {
      ggplot2::scale_fill_discrete(guide = if (show_fill) ggplot2::guide_legend(order = 1) else "none")
    } else {
      ggplot2::scale_fill_continuous(guide = if (show_fill) ggplot2::guide_colourbar(order = 1) else "none")
    }
  }

  # Annotation for rows and columns
  # Default annotation parameters
  annot_default <- list(legend = annot_legend, dist = annot_dist, gap = annot_gap, size = annot_size,
                        label = annot_label, border_col = annot_border_col, border_lwd = annot_border_lwd,
                        border_lty = annot_border_lty, na_col = annot_na_col)
  if (is.data.frame(annot_rows_df)) {
    annot_rows_prep <- prepare_annotation(annot_df = annot_rows_df, annot_defaults = annot_default,
                                          annot_params = annot_rows_params, lannot_side = annot_left,
                                          annot_label_params = annot_rows_label_params,
                                          annot_label_side = annot_rows_label_side, data_size = ncol(x_mat))
    annot_rows_df <- annot_rows_prep[[1]]; annot_rows_params <- annot_rows_prep[[2]];
    annot_rows_pos <- annot_rows_prep[[3]]; annot_rows_label_params <- annot_rows_prep[[4]]

    # Check that names in annotation exist in the plotting data
    if (any(!annot_rows_df[[".names"]] %in% x_long[["row"]])) {
      bad_names <- setdiff(annot_rows_df[[".names"]], x_long[["row"]])
      stop("Some names in the row annotation don't exist in the data: ",
           paste(bad_names, collapse = ", "))
    }
  }

  if (is.data.frame(annot_cols_df)) {
    annot_cols_prep <- prepare_annotation(annot_df = annot_cols_df, annot_defaults = annot_default,
                                          annot_params = annot_cols_params, lannot_side = annot_down,
                                          annot_label_params = annot_cols_label_params,
                                          annot_label_side = annot_cols_label_side, data_size = nrow(x_mat))
    annot_cols_df <- annot_cols_prep[[1]]; annot_cols_params <- annot_cols_prep[[2]];
    annot_cols_pos <- annot_cols_prep[[3]]; annot_cols_label_params <- annot_cols_prep[[4]]

    if (any(!annot_cols_df[[".names"]] %in% x_long[["col"]])) {
      bad_names <- setdiff(annot_cols_df[[".names"]], x_long[["col"]])
      stop("Some names in the column annotation don't exist in the data: ",
           paste(bad_names, collapse = ", "))
    }
  }

  # Generate dendrograms, positions depend on annotation sizes
  dend_defaults <- list(col = dend_col, height = dend_height, lwd = dend_lwd, lty = dend_lty)
  if (lclust_rows & dend_rows) {
    # Replace default parameters if any are provided
    dend_rows_params <- replace_default(dend_defaults, dend_rows_params)

    dendro_rows <- prepare_dendrogram(dendro_in = row_clustering$dendro, dend_dim = "rows",
                                      dend_down = dend_down, dend_left = dend_left,
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
                                      dend_height = dend_cols_params$height,
                                      full_plt = full_plt, layout = layout, x_long = x_long,
                                      annot_df = annot_cols_df, annot_side = annot_down,
                                      annot_pos = annot_cols_pos, annot_size = annot_cols_params$size)
    dendro_cols <- check_dendrogram_pos(x_long, "col", dendro_cols)
  }

  # Build plot
  if (length(layout) == 1) {
    plt <- make_heatmap(x_long = x_long, plt = NULL, mode = mode, layout = layout, include_diag = include_diag,
                        invisible_diag = isSymmetric(x_mat) & !include_diag,
                        border_lwd = border_lwd, border_col = border_col, border_lty = border_lty,
                        names_diag = names_diag, names_x = names_x, names_y = names_y,
                        names_x_side = names_x_side, names_y_side = names_y_side,
                        names_diag_param = names_diag_param, show_legend = show_legend,
                        fill_scale = fill_scale, fill_name = fill_name,
                        col_scale = col_scale, col_name = col_name, size_scale = size_scale,
                        cell_labels = cell_labels, cell_label_col = cell_label_col,
                        cell_label_size = cell_label_size, cell_label_digits = cell_label_digits)
  } else if (length(layout) == 2) {
    # First half of the plot
    plt <- make_heatmap(x_long = dplyr::filter(x_long, lt == layout[1]), plt = NULL,
                        mode = mode[1], layout = layout[1],
                        include_diag = include_diag, invisible_diag = !include_diag,
                        border_lwd = border_lwd[[1]], border_col = border_col[[1]], border_lty = border_lty[[1]],
                        names_diag = names_diag, names_x = names_x, names_y = names_y,
                        names_x_side = names_x_side, names_y_side = names_y_side,
                        names_diag_param = names_diag_param, show_legend = show_legend,
                        fill_scale = fill_scale, fill_name = fill_name,
                        col_scale = col_scale, col_name = col_name, size_scale = size_scale,
                        cell_labels = cell_labels[[1]], cell_label_col = cell_label_col[[1]],
                        cell_label_size = cell_label_size[[1]], cell_label_digits = cell_label_digits[[1]])
    # Remaining half
    plt <- make_heatmap(x_long = dplyr::filter(x_long, lt == layout[2]), plt = plt,
                        mode = mode[2], layout = layout[2],
                        include_diag = F, invisible_diag = F,
                        border_lwd = border_lwd[[2]], border_col = border_col[[2]], border_lty = border_lty[[2]],
                        names_diag = F, names_x = names_x, names_y = names_y,
                        names_x_side = names_x_side, names_y_side = names_y_side, show_legend = show_legend,
                        names_diag_param = names_diag_param,
                        fill_scale = NULL, fill_name = fill_name, col_scale = NULL, col_name = col_name, size_scale = NULL,
                        cell_labels = cell_labels[[2]], cell_label_col = cell_label_col[[2]],
                        cell_label_size = cell_label_size[[2]], cell_label_digits = cell_label_digits[[2]])
  }

  # Prepare default colour scales
  annot_col_list <- prepare_annot_col(annot_rows_df, annot_cols_df, annot_rows_fill, annot_cols_fill)

  # Add row and column annotations
  if (is.data.frame(annot_rows_df)) {
    # Change legend order to be in order of annotations (after main values)
    lgd_order <- seq_along(annot_rows_df)[-1]
    names(lgd_order) <- colnames(annot_rows_df)[-which(colnames(annot_rows_df) == ".names")]

    plt <- add_annotation(plt = plt, annot_dim = "rows", annot_df = annot_rows_df, annot_pos = annot_rows_pos,
                          annot_size = annot_rows_params$size, annot_border_lwd = annot_rows_params$border_lwd,
                          annot_border_col = annot_rows_params$border_col, annot_border_lty = annot_rows_params$border_lty,
                          draw_legend = annot_rows_params$legend, draw_label = annot_rows_params$label,
                          na_col = annot_na_col, na_remove = annot_na_remove, col_scale = annot_col_list[[1]],
                          legend_order = lgd_order, label_side = annot_rows_label_side, label_params = annot_rows_label_params)
  }

  if (is.data.frame(annot_cols_df)) {
    lgd_order <- if (is.data.frame(annot_rows_df)) {seq_along(annot_cols_df)[-1] + ncol(annot_rows_df) - 1}
    else {seq_along(annot_cols_df)[-1]}
    names(lgd_order) <- colnames(annot_cols_df)[-which(colnames(annot_cols_df) == ".names")]

    plt <- add_annotation(plt = plt, annot_dim = "cols", annot_df = annot_cols_df, annot_pos = annot_cols_pos,
                          annot_size = annot_cols_params$size, annot_border_lwd = annot_cols_params$border_lwd,
                          annot_border_col = annot_cols_params$border_col, annot_border_lty = annot_cols_params$border_lty,
                          draw_legend = annot_cols_params$legend, draw_label = annot_cols_params$label,
                          na_col = annot_na_col, na_remove = annot_na_remove, col_scale = annot_col_list[[2]],
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


#' Prepare parameters for mixed layouts.
#'
#' @keywords internal
#'
#' @param param Parameter to prepare.
#' @param param_name Parameter name (for error messages).
#'
#' @returns If length one, it is returned duplicated in a list for use in each triangle. If longer than 1
#' an error message is returned. In other cases, the input parameter is returned (length two input).
#'
prepare_mixed_param <- function(param, param_name) {
  if (length(param) == 1) {
    param_out <- list(param, param)
  } else if (length(param) != 2) {
    stop(param_name, " must be either a vector length one (used for all cells) or\nlength two (used for each triangle), or a list of length two where each\nelement is a vector with values for the cells in the triangle.")
  } else {
    param_out <- param
  }

  return(param_out)
}
