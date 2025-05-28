#' Make a heatmap with ggplot2.
#'
#' @param x Matrix or data frame in wide format to make a heatmap of. If rownames are present they are used for the y axis labels, otherwise the row number is used.
#' If a column named `.names` (containing unique row identifiers) is present it will be used as rownames.
#' @param fill_scale A `ggplot2` scale object for the cell fill colour scale. NULL (default) uses the `ggplot2` default.
#' @param fill_name String to use for the colour scale legend title.
#' @param na_remove Logical indicating if NA values in the heatmap should be omitted (meaning no cell border is drawn).
#' If NAs are kept, the fill colour can be set in the `ggplot2` scale.
#' @param layout String specifying the layout of the output heatmap. Possible layouts include
#' top left, top right, bottom left, bottom right, or the whole heatmap (default and only possible option if the matrix is asymmetric).
#' The string should be composed of the vertical position (top or bottom) followed by the horizontal position (left or right).
#' Bottom can be specified by 'bottom', 'lower', 'down', or the first letter of these. Left is specified by 'left' or 'l'.
#' 'full', 'whole', or 'all' (or 'f', 'w', 'a') result in the whole matrix being plotted.
#' For any other strings top and right are selected.
#' @param include_diag Logical indicating if the diagonal cells should be plotted (ignored if the whole matrix is plotted).
#' Mostly only useful for getting a cleaner look with symmetric correlation matrices with triangular layouts, where the diagonal is known to be 1.
#' @param return_data Logical indicating if the data used for plotting should be returned.
#' @param show_legend Logical vector indicating if main heatmap legends (fill and size) should be shown. If length 1 it is applied to both fill and size legends,
#' can be specified in an aesthetic-specific manner using a named vector like `c('fill' = TRUE, 'size' = FALSE)`.
#' @param cell_shape Value specifying what shape the heatmap cells should take. Any non-numeric value will result in a normal heatmap with square cells (default).
#' A numeric value can be used to specify an R shape (pch) to use, such as 21 for filled circles. Note that only shapes 21-25 support filling (others will not display the heatmap colour properly).
#' @param size_scale `ggplot2::scale_size_*` call to use for size scaling if `cell_shape` is numeric.
#' @param label_cells Logical specifying if the cells should be labelled with the values.
#' @param cell_label_size Size of cell labels, used as the `size` argument in `ggplot2::geom_text`.
#' @param cell_label_digits Number of digits to display when cells are labelled (if numeric values). Default is 2, passed to `round`. NULL for no rounding.
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
#' @return The heatmap as a `ggplot` object.
#' If `return_data` is TRUE the output is a list containing the plot (named 'plot'),
#' the plotting data ('plot_data'), and the result of the clustering ('clustering', only if `cluster_data` is TRUE).
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
#' # Use part of the mtcars data (for visibility)
#' hm_in <- mtcars[1:15, ]
#'
#' # Basic usage
#' gghm(hm_in)
#'
#' # Different layout (using a symmetric matrix)
#' gghm(cor(mtcars), layout = "br")
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
#'      annot_rows_df = annot_rows, annot_rows_fill = annot_fill,
#'      # Change margins to fit annotation labels,
#'      plot_margin = c(20, 10, 60, 20))
#'
#' # Using the dend_options argument
#' gghm(scale(hm_in), cluster_rows = TRUE, dend_rows_extend =
#'   list("set" = list("branches_lty", c(1, 2, 3)),
#'        # Empty list element (or NULL) if no arguments to be given
#'        "highlight_branches_col" = list()))
gghm <- function(x, fill_scale = NULL, fill_name = "value", na_remove = FALSE,
                 layout = "full", include_diag = F, return_data = F,
                 show_legend = c("fill" = TRUE, "size" = TRUE), cell_shape = "heatmap", size_scale = NULL,
                 label_cells = F, cell_label_size = 3, cell_label_digits = NULL,
                 border_col = "grey", border_lwd = 0.5,
                 names_diag = TRUE, names_diag_param = NULL,
                 names_x = FALSE, names_x_side = "top", names_y = FALSE, names_y_side = "left",
                 annot_rows_df = NULL, annot_cols_df = NULL, annot_rows_fill = NULL, annot_cols_fill = NULL,
                 annot_rows_side = "right", annot_cols_side = "bottom",
                 annot_legend = TRUE, annot_dist = 0.2, annot_gap = 0, annot_size = 0.5, annot_label = TRUE,
                 annot_rows_params = NULL, annot_cols_params = NULL,
                 annot_rows_label_side = "bottom", annot_cols_label_side = "left",
                 annot_rows_label_params = NULL, annot_cols_label_params = NULL,
                 cluster_rows = FALSE, cluster_cols = FALSE,
                 cluster_distance = "euclidean", cluster_method = "complete",
                 dend_rows = TRUE, dend_cols = TRUE, dend_rows_side = "right", dend_cols_side = "bottom",
                 dend_col = "black", dend_height = 0.3, dend_lwd = 0.3, dend_lty = 1,
                 dend_rows_params = NULL, dend_cols_params = NULL,
                 dend_rows_extend = NULL, dend_cols_extend = NULL,
                 legend_position = "right",
                 plot_margin = c(20, 10, 10, 20), margin_unit = "pt") {

  if (!is.matrix(x) & !is.data.frame(x)) stop("x must be a matrix or data frame.")

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

  full_plt <- grepl("full|whole|all|^a|^w|^f", layout)

  # If the matrix is non-symmetric, triangular layouts break! Throw a warning
  if (!isSymmetric(x_mat) & !full_plt) {
    warning("A triangular layout with an asymmetric matrix is not supported, plotting the full matrix instead.")
    full_plt <- T
  }

  # Don't display names on the diagonal if the plot is non-symmetric as it will cause
  # new ghost columns to be added to draw the names where row == col
  if (!isSymmetric(x_mat)) {
    names_diag <- F
    # Also display x and y names by default, but remove if specified as FALSE (when specified as a named argument)
    names_x <- eval(replace_default(list("names_x" = T), as.list(sys.call()))$names_x)
    names_y <- eval(replace_default(list("names_y" = T), as.list(sys.call()))$names_y)
  }

  # If clustering a symmetric matrix with a triangular layout, both rows and columns must be clustered. Automatically cluster both and throw a warning
  if (isSymmetric(x_mat) & !full_plt &
      ((!isFALSE(cluster_rows) & isFALSE(cluster_cols)) | (isFALSE(cluster_rows) & !isFALSE(cluster_cols)))) {
    warning("Cannot cluster only one dimension for triangular layouts with symmetric matrices, clustering both rows and columns.")
    if (isFALSE(cluster_rows)) {cluster_rows <- cluster_cols}
    else if (isFALSE(cluster_cols)) {cluster_cols <- cluster_rows}
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

  # Reorder rows and columns depending on clustering
  # Different ways to reorder depending on position of triangular matrix, default to bottom right if no correct pattern is recognised
  # Allow for specification of dendrogram positions only when the whole matrix is drawn

  # Make long format data
  x_long <- shape_mat_long(x_mat, unique_pairs = !full_plt, na_remove = na_remove)

  if (full_plt) {
    dend_left <- grepl("left", dend_rows_side)
    dend_down <- grepl("lower|bottom|down", dend_cols_side)
    annot_left <- grepl("left", annot_rows_side)
    annot_down <- grepl("lower|bottom|down", annot_cols_side)
    include_diag <- T
  } else {
    pos_left = dend_left = annot_left <- grepl("left", layout) | substring(layout, 2, 2) == "l"
    pos_down = dend_down = annot_down <- grepl("lower|bottom|down", layout) | substring(layout, 1, 1) %in% c("l", "b", "d")
  }

  x_long$row <- factor(x_long$row,
                       levels = if (full_plt) {
                         if (lclust_rows) {row_clustering$dendro$labels$label} else {rev(rownames(x_mat))}
                       } else if (pos_down) {
                         if (lclust_rows) {rev(row_clustering$dendro$labels$label)} else {rev(rownames(x_mat))}
                       } else {
                         if (lclust_rows) {row_clustering$dendro$labels$label} else {rownames(x_mat)}
                       })
  x_long$col <- factor(x_long$col,
                       levels = if (full_plt) {
                         if (lclust_cols) {rev(col_clustering$dendro$labels$label)} else {colnames(x_mat)}
                       } else if (!pos_left) {
                         if (lclust_cols) {rev(col_clustering$dendro$labels$label)} else {rev(colnames(x_mat))}
                       } else {
                         if (lclust_cols) {col_clustering$dendro$labels$label} else {colnames(x_mat)}
                       })

  # Annotation for rows and columns
  if (is.data.frame(annot_rows_df)) {
    # Move names to column if in row names
    if (!".names" %in% colnames(annot_rows_df)) {
      annot_rows_df$.names <- rownames(annot_rows_df)
      rownames(annot_rows_df) <- NULL
    }
    annot_rows_names <- colnames(annot_rows_df)[-which(colnames(annot_rows_df) == ".names")]

    # Make list with annotation parameter defaults from the common annotation options
    annot_rows_defaults <- list(legend = annot_legend, dist = annot_dist, gap = annot_gap,
                                size = annot_size, label = annot_label,
                                border_col = border_col, border_lwd = border_lwd)
    # Replace defaults with any provided options
    annot_rows_params <- replace_default(annot_rows_defaults, annot_rows_params)

    # Get positions of annotations
    annot_rows_pos <- get_annotation_pos(annot_left, annot_rows_names, annot_rows_params$size,
                                         annot_rows_params$dist, annot_rows_params$gap, ncol(x_mat))

    # Row annotation label parameters and their defaults (fed to grid::textGrob)
    annot_rows_label_defaults <- list(rot = 90, just = switch(annot_rows_label_side, "bottom" = "right", "top" = "left"))
    annot_rows_label_params <- replace_default(annot_rows_label_defaults, annot_rows_label_params)
  }

  if (is.data.frame(annot_cols_df)) {
    # Move names to column if in row names
    if (!".names" %in% colnames(annot_cols_df)) {
      annot_cols_df$.names <- rownames(annot_cols_df)
      rownames(annot_cols_df) <- NULL
    }
    annot_cols_names <- colnames(annot_cols_df)[-which(colnames(annot_cols_df) == ".names")]

    # Make list with annotation parameter defaults from the common annotation options
    annot_cols_defaults <- list(legend = annot_legend, dist = annot_dist, gap = annot_gap,
                                size = annot_size, label = annot_label,
                                border_col = border_col, border_lwd = border_lwd)
    # Replace defaults with any provided options
    annot_cols_params <- replace_default(annot_cols_defaults, annot_cols_params)

    annot_cols_pos <- get_annotation_pos(annot_down, annot_cols_names, annot_cols_params$size,
                                         annot_cols_params$dist, annot_cols_params$gap, nrow(x_mat))

    annot_cols_label_defaults <- list(rot = 0, just = switch(annot_cols_label_side, "left" = "right", "right" = "left"))
    annot_cols_label_params <- replace_default(annot_cols_label_defaults, annot_cols_label_params)
  }

  # Generate dendrograms
  if (lclust_rows & dend_rows) {

    dend_rows_defaults <- list(col = dend_col, height = dend_height, lwd = dend_lwd, lty = dend_lty)
    # Replace default parameters if any are provided
    dend_rows_params <- replace_default(dend_rows_defaults, dend_rows_params)

    dend_seg_rows <- prepare_dendrogram(row_clustering$dendro, "rows", dend_down, dend_left, dend_rows_params$height, full_plt, x_long,
                                        annot_rows_df, is.data.frame(annot_rows_df), annot_left, annot_rows_pos, annot_rows_params$size)

    # Check that the dendrogram labels are in the correct positions after mirroring and shifting
    dend_seg_rows <- check_dendrogram_pos(x_long, "row", dend_seg_rows)
  }

  if (lclust_cols & dend_cols) {

    dend_cols_defaults <- list(col = dend_col, height = dend_height, lwd = dend_lwd, lty = dend_lty)
    # Replace default parameters if any are provided
    dend_cols_params <- replace_default(dend_cols_defaults, dend_cols_params)

    dend_seg_cols <- prepare_dendrogram(col_clustering$dendro, "cols", dend_down, dend_left, dend_cols_params$height, full_plt, x_long,
                                        annot_cols_df, is.data.frame(annot_cols_df), annot_down, annot_cols_pos, annot_cols_params$size)
    dend_seg_cols <- check_dendrogram_pos(x_long, "col", dend_seg_cols)
  }

  # Start building plot
  plt <- ggplot2::ggplot(mapping = ggplot2::aes(x = col, y = row))
  # Draw diagonal first to draw over with the rest of the plot (only if symmetric matrix)
  if (isSymmetric(x_mat)) {
    if (include_diag) {
      plt <- plt +
        # Tiles or not
        list(
          if (is.numeric(cell_shape)) {
            ggplot2::geom_point(ggplot2::aes(fill = value, size = value),
                                # Subset on character columns as error occurs if factor levels are different
                                subset(x_long, as.character(row) == as.character(col)),
                                stroke = border_lwd, colour = border_col, shape = cell_shape,
                                show.legend = show_legend)
          } else {
            ggplot2::geom_tile(ggplot2::aes(fill = value),
                               subset(x_long, as.character(row) == as.character(col)),
                               linewidth = border_lwd, colour = border_col, show.legend = show_legend)
          }
        )

    } else {
      # Draw diagonal even if not supposed to be included to get the positions into the plot, for easier labelling
      plt <- plt +
        ggplot2::geom_tile(data = subset(x_long, as.character(row) == as.character(col)),
                           fill = "white", linewidth = 0, alpha = 0)
    }
  }

  # If a numeric vector is given for the legend position, use the "inside" value for the legend.position arugment
  # and give the vector as input for the legend.position.inside argument in the theme() function
  if (is.numeric(legend_position)) {
    legend_position_inside <- legend_position
    legend_position <- "inside"
  } else {
    legend_position_inside <- NULL
  }

  plt <- plt +
    # Plot tiles or points depending on cell_shape
    list(
      if (is.numeric(cell_shape)) {
        ggplot2::geom_point(ggplot2::aes(fill = value, size = value),
                            data = if (isSymmetric(x_mat)) {
                              subset(x_long, as.character(row) != as.character(col))
                            } else {
                              x_long
                            },
                            stroke = border_lwd, colour = border_col, shape = cell_shape,
                            show.legend = show_legend)
      } else {
        ggplot2::geom_tile(ggplot2::aes(fill = value),
                           data = if (isSymmetric(x_mat)) {
                             subset(x_long, as.character(row) != as.character(col))
                           } else {
                             x_long
                           },
                           linewidth = border_lwd, colour = border_col, show.legend = show_legend)
      }
    ) +
    size_scale +
    # Remove extra space on axes (if drawing tiles) and place on specified sides
    ggplot2::scale_x_discrete(expand = if (is.numeric(cell_shape)) c(.05, .05) else c(0, 0), position = names_x_side) +
    ggplot2::scale_y_discrete(expand = if (is.numeric(cell_shape)) c(.05, .05) else c(0, 0), position = names_y_side) +
    # Add colour
    fill_scale +
    # Make cells square
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::labs(fill = fill_name) +
    ggplot2::theme_classic() +
    # Remove axis elements, move legend, set margins
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text.x = if (names_x) ggplot2::element_text() else ggplot2::element_blank(),
                   axis.text.y = if (names_y) ggplot2::element_text() else ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   legend.position = legend_position, legend.position.inside = legend_position_inside,
                   plot.margin = ggplot2::margin(plot_margin[1], plot_margin[2], plot_margin[3], plot_margin[4], margin_unit))

  # Names on the diagonal
  if (names_diag) {
    axis_lab <- data.frame(lab = levels(x_long$row))

    # Construct call using optional parameters
    text_call_params <- list(data = axis_lab, mapping = ggplot2::aes(x = lab, y = lab, label = lab))
    if (is.list(names_diag_param)) {
      text_call_params <- append(text_call_params, names_diag_param)
    }

    plt <- plt +
      do.call(ggplot2::geom_text, text_call_params)
  }

  # Cell labels
  if (label_cells) {
    # Don't draw in the diagonal cells if are hidden or if they are already occupied by names
    cell_data <- if (include_diag & !names_diag) x_long else subset(x_long, as.character(row) != as.character(col))

    plt <- plt +
      ggplot2::geom_text(data = cell_data,
                         mapping = ggplot2::aes(x = col, y = row,
                                                label = if (!is.null(cell_label_digits)) {round(value, cell_label_digits)} else {value}),
                         size = cell_label_size)
  }

  # Prepare default colour scales
  annot_col_list <- prepare_annot_col(annot_rows_df, annot_cols_df, annot_rows_fill, annot_cols_fill)

  # Add row and column annotations
  if (is.data.frame(annot_rows_df)) {
    # Change legend order to be in order of annotations (after main values)
    lgd_order <- seq_along(annot_rows_df)[-1]
    names(lgd_order) <- colnames(annot_rows_df)[-1]

    plt <- add_annotation(plt, annot_dim = "rows", annot_rows_df, annot_rows_pos, annot_rows_params$size,
                          annot_rows_params$border_lwd, annot_rows_params$border_col, annot_rows_params$legend,
                          annot_col_list[[1]], lgd_order, annot_rows_label_side, annot_rows_label_params)
  }

  if (is.data.frame(annot_cols_df)) {
    lgd_order <- if (is.data.frame(annot_rows_df)) {seq_along(annot_cols_df)[-1] + ncol(annot_rows_df) - 1}
    else {seq_along(annot_cols_df)[-1]}
    names(lgd_order) <- colnames(annot_cols_df)[-1]

    plt <- add_annotation(plt, annot_dim = "cols", annot_cols_df, annot_cols_pos, annot_cols_params$size,
                          annot_cols_params$border_lwd, annot_cols_params$border_col, annot_cols_params$legend,
                          annot_col_list[[2]], lgd_order, annot_cols_label_side, annot_cols_label_params)
  }

  # Add dendrograms
  if (lclust_rows & dend_rows) {
    plt <- add_dendrogram(plt, dend_seg_rows, dend_rows_params$col, dend_rows_params$lwd, dend_rows_params$lty)
  }

  if (lclust_cols & dend_cols) {
    plt <- add_dendrogram(plt, dend_seg_cols, dend_cols_params$col, dend_cols_params$lwd, dend_cols_params$lty)
  }

  if (return_data) {

    list_out <- list("plot" = plt, "plot_data" = x_long)

    if (!isFALSE(cluster_rows)) {
      # list_out$row_clustering <- row_clustering$clust
      list_out$row_clustering <- dend_seg_rows
    }
    if (!isFALSE(cluster_cols)) {
      # list_out$col_clustering <- col_clustering$clust
      list_out$col_clustering <- dend_seg_cols
    }

    return(list_out)

  } else {
    return(plt)
  }
}


