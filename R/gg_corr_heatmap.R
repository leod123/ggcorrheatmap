#' Make a correlation heatmap with ggplot2.
#'
#' @param data A matrix or data frame containing numeric values, where the columns will be correlated with each other.
#' @param cor_method String specifying correlation method to use in the `cor` function. Default is 'pearson'.
#' @param cor_use String specifying the `use` argument of `cor`, which defineshow to deal with missing values. Default is 'everything'.
#' @param high Name of the colour to use for the highest value of the colour scale.
#' @param mid Name of the colour to use for 0 in the colour scale.
#' @param low Name of the colour to use for the lowest value of the colour scale.
#' @param limits Correlation limits to plot between.
#' @param bins Specify number of bins if the correlation scale should be binned. NULL for a continuous scale.
#' @param layout String specifying the layout of the output correlation heatmap. Possible layouts include
#' top left, top right, bottom left, bottom right (default), or the whole heatmap. The string should be composed
#' of the vertical position (top or bottom) followed by the horizontal position (left or right). Bottom can
#' be specified by 'bottom', 'lower', 'down', or the first letter of these. Left is specified by 'left' or 'l'.
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
#' @param params_diag List with named parameters (such as size, angle, etc) passed on to geom_text when writing the column names in the diagonal.
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
#' @param annot_label_size not yet implemented
#' @param annot_rows_params Named list with parameters for row annotations to overwrite the defaults set by the `annot_*` arguments, each name corresponding to the `*` part
#' (see details for more information).
#' @param annot_cols_params Named list with parameters for column annotations, used like `annot_rows_params`.
#' @param cluster_data Logical indicating if the correlations should be clustered in the heatmap. May also be
#' a `hclust` object where the correlations have been clustered already.
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
#' @param dend_options A named list with options to give to `dendextend::set` for customising the dendrogram. See details for usage.
#' @param legend_position Position of the legends (use how???)
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
#' The `dend_options` argument makes it possible to customise the dendrograms using the `dendextend` package.
#' The argument should be a named list, each element named after the `dendextend` function to use (consecutive usage of the `set` function
#' is supported due to duplicate list names being possible). Each element should contain any arguments given to the `dendextend` function,
#' such as the `what` argument used in the `set` function. See examples for example usage.
#'
#' @examples
#' # Basic usage
#' gg_corr_heatmap(mtcars)
#'
#' # Different layout
#' gg_corr_heatmap(mtcars, layout = "f")
#'
#' # With clustering
#' gg_corr_heatmap(mtcars, layout = "tl", cluster_data = T)
#'
#' # With annotation
#' set.seed(123)
#' annot <- data.frame(.names = colnames(mtcars),
#'                     annot1 = rnorm(ncol(mtcars)),
#'                     annot2 = sample(letters[1:3], ncol(mtcars), T))
#' gg_corr_heatmap(mtcars, layout = "tr", annot_rows_df = annot)
#'
#' # Using the dend_options argument
#' gg_corr_heatmap(mtcars, cluster_data = T, dend_options =
#'   list("set" = list("branches_lty", c(1, 2, 3)),
#'        "set" = list("branches_k_color", k = 3),
#'        # Empty list element (or NULL) if no arguments to be given
#'        "highlight_branches_lwd" = list()))
gg_corr_heatmap <- function(data, cor_method = "pearson", cor_use = "everything",
                            high = "sienna2", mid = "white", low = "skyblue2", limits = c(-1, 1), bins = NULL,
                            layout = "lowerright", include_diag = F, return_data = F,
                            cell_shape = "heatmap", label_cells = F, cell_label_size = 3, cell_label_digits = 2,
                            border_col = "grey", border_lwd = 0.5,
                            names_diag = T, params_diag = NULL,
                            names_x = F, names_x_side = "top", names_y = F, names_y_side = "left",
                            annot_rows_df = NULL, annot_cols_df = NULL, annot_rows_fill = NULL, annot_cols_fill = NULL,
                            annot_rows_side = "right", annot_cols_side = "bottom",
                            annot_legend = T, annot_dist = 0.2, annot_gap = 0, annot_size = 0.5, annot_label = T, annot_label_size = 4,
                            annot_rows_params = NULL, annot_cols_params = NULL,
                            cluster_data = F, cluster_distance = "euclidean", cluster_method = "complete",
                            dend_rows = T, dend_cols = T, dend_rows_side = "right", dend_cols_side = "bottom",
                            dend_col = "black", dend_height = 0.3, dend_lwd = 0.3, dend_lty = 1,
                            dend_rows_params = NULL, dend_cols_params = NULL,
                            dend_options = NULL,
                            legend_position = "right",
                            plot_margin = c(20, 10, 10, 20), margin_unit = "pt") {

  cor_mat <- cor(data, method = cor_method, use = cor_use)

  # Make dendrograms
  # To allow for providing a hclust object when clustering, make a separate logical clustering variable
  if (is.logical(cluster_data)) {
    if (cluster_data) {
      lclust_data <- T

      clust <- hclust(dist(cor_mat, method = cluster_distance), method = cluster_method)

      dendro <- as.dendrogram(clust)

      # Apply dendextend options if any are given
      if (is.list(dend_options)) {
        dendro <- apply_dendextend(dendro, dend_options)
      }

      dendro <- dendextend::as.ggdend(dendro)
      dendro$labels$label <- as.character(dendro$labels$label)

      cor_mat <- cor_mat[rev(dendro$labels$label), rev(dendro$labels$label)]

    } else {
      lclust_data <- F
    }


  } else if (class(cluster_data) == "hclust") {
    lclust_data <- T
    clust <- cluster_data
    dendro <- as.dendrogram(clust)

    # Apply dendextend options if any
    if (is.list(dend_options)) {
      dendro <- apply_dendextend(dendro, dend_options)
    }

    dendro <- dendextend::as.ggdend(dendro)
    dendro$labels$label <- as.character(dendro$labels$label)

    cor_mat <- cor_mat[rev(dendro$labels$label), rev(dendro$labels$label)]

  } else {
    lclust_data <- F
  }

  # Make correlation matrix and long-format triangular correlation matrix
  cor_long <- cor_shape_long(cor_mat, unique_pairs = !grepl("full|whole|all|^a|^w|^f", layout))

  # Reorder rows and columns depending on clustering
  # Different ways to reorder depending on position of triangular matrix, default to bottom right if no correct pattern is recognised
  # Allow for specification of dendrogram positions only when the whole matrix is drawn
  full_plt <- grepl("full|whole|all|^a|^w|^f", layout)
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

  cor_long$row <- factor(cor_long$row,
                         levels = if (full_plt) {
                           if (lclust_data) {rev(dendro$labels$label)} else {colnames(data)}
                         } else if (pos_left) {
                           if (lclust_data) {dendro$labels$label} else {rev(colnames(data))}
                         } else {
                           if (lclust_data) {rev(dendro$labels$label)} else {colnames(data)}
                         })
  cor_long$col <- factor(cor_long$col,
                         levels = if (full_plt) {
                           if (lclust_data) {rev(dendro$labels$label)} else {colnames(data)}
                         } else if (!pos_down) {
                           if (lclust_data) {dendro$labels$label} else {rev(colnames(data))}
                         } else {
                           if (lclust_data) {rev(dendro$labels$label)} else {colnames(data)}
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
                                size = annot_size, label = annot_label, label_size = annot_label_size,
                                border_col = border_col, border_lwd = border_lwd)
    # Replace defaults with any provided options
    annot_rows_params <- replace_default(annot_rows_defaults, annot_rows_params)

    # Get positions of annotations
    annot_rows_pos <- get_annotation_pos("rows", annot_left, annot_rows_names, annot_rows_params$size,
                                         annot_rows_params$dist, annot_rows_params$gap, ncol(data))
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
                                size = annot_size, label = annot_label, label_size = annot_label_size,
                                border_col = border_col, border_lwd = border_lwd)
    # Replace defaults with any provided options
    annot_cols_params <- replace_default(annot_cols_defaults, annot_cols_params)

    annot_cols_pos <- get_annotation_pos("cols", annot_down, annot_cols_names, annot_cols_params$size,
                                         annot_cols_params$dist, annot_cols_params$gap, ncol(data))
  }

  # Generate dendrograms
  if (lclust_data & dend_rows) {

    dend_rows_defaults <- list(col = dend_col, height = dend_height, lwd = dend_lwd, lty = dend_lty)
    # Replace default parameters if any are provided
    dend_rows_params <- replace_default(dend_rows_defaults, dend_rows_params)

    dend_seg_rows <- prepare_dendrogram(dendro, "rows", dend_down, dend_left, dend_rows_params$height, full_plt, cor_long,
                                        annot_rows_df, is.data.frame(annot_rows_df), annot_left, annot_rows_pos, annot_rows_params$size)
  }

  if (lclust_data & dend_cols) {

    dend_cols_defaults <- list(col = dend_col, height = dend_height, lwd = dend_lwd, lty = dend_lty)
    # Replace default parameters if any are provided
    dend_cols_params <- replace_default(dend_cols_defaults, dend_cols_params)

    dend_seg_cols <- prepare_dendrogram(dendro, "cols", dend_down, dend_left, dend_cols_params$height, full_plt, cor_long,
                                        annot_cols_df, is.data.frame(annot_cols_df), annot_down, annot_cols_pos, annot_cols_params$size)
  }

  # Start building plot
  cor_plt <- ggplot2::ggplot(mapping = ggplot2::aes(x = row, y = col))
  # Draw diagonal first to draw over with the rest of the plot
  if (include_diag) {
    cor_plt <- cor_plt +
      # Tiles or not
      list(
        if (is.numeric(cell_shape)) {
          ggplot2::geom_point(ggplot2::aes(fill = cor, size = abs(cor)), subset(cor_long, row == col),
                              stroke = border_lwd, colour = border_col, shape = cell_shape,
                              show.legend = c("fill" = T, "size" = F))
        } else {
          ggplot2::geom_tile(ggplot2::aes(fill = cor), subset(cor_long, row == col),
                             linewidth = border_lwd, colour = border_col)
        }
      )

  } else {
    # Draw diagonal even if not supposed to be included to get the positions into the plot, for easier labelling
    cor_plt <- cor_plt +
      ggplot2::geom_tile(data = subset(cor_long, row == col), fill = "white", linewidth = 0, alpha = 0)
  }

  # If a numeric vector is given for the legend position, use the "inside" value for the legend.position arugment
  # and give the vector as input for the legend.position.inside argument in the theme() function
  if (is.numeric(legend_position)) {
    legend_position_inside <- legend_position
    legend_position <- "inside"
  } else {
    legend_position_inside <- NULL
  }

  cor_plt <- cor_plt +
    # Plot tiles or points depending on cell_shape
    list(
      if (is.numeric(cell_shape)) {
        ggplot2::geom_point(ggplot2::aes(fill = cor, size = abs(cor)), subset(cor_long, row != col),
                            stroke = border_lwd, colour = border_col, shape = cell_shape,
                            show.legend = c("fill" = T, "size" = F))
      } else {
        ggplot2::geom_tile(ggplot2::aes(fill = cor), subset(cor_long, row != col),
                           linewidth = border_lwd, colour = border_col)
      }
    ) +
    # Remove extra space on axes (if drawing tiles) and place on specified sides
    ggplot2::scale_x_discrete(expand = if (is.numeric(cell_shape)) c(.05, .05) else c(0, 0), position = names_x_side) +
    ggplot2::scale_y_discrete(expand = if (is.numeric(cell_shape)) c(.05, .05) else c(0, 0), position = names_y_side) +
    # Add colour
    list(if (!is.null(bins)) {
      ggplot2::scale_fill_steps2(limits = limits, high = high, mid = mid, low = low,
                                 breaks = seq(limits[1], limits[2], length.out = bins), guide = ggplot2::guide_colourbar(order = 1))
    } else {
      ggplot2::scale_fill_gradient2(limits = limits, high = high, mid = mid, low = low,
                                    guide = ggplot2::guide_colourbar(order = 1))
    }) +
    # Make cells square
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::labs(fill = switch(cor_method,
                                "pearson" = "Pearson r",
                                "spearman" = "Spearman\nrho",
                                "kendall" = "Kendall\ntau")) +
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
    axis_lab <- data.frame(lab = levels(cor_long$row))

    # Construct call using optional parameters
    text_call_params <- list(data = axis_lab, mapping = ggplot2::aes(x = lab, y = lab, label = lab))
    if (is.list(params_diag)) {
      text_call_params <- append(text_call_params, params_diag)
    }

    cor_plt <- cor_plt +
      do.call(ggplot2::geom_text, text_call_params)
  }

  # Cell labels
  if (label_cells) {
    # Don't draw in the diagonal cells if are hidden or if they are already occupied by names
    cell_data <- if (include_diag & !names_diag) cor_long else subset(cor_long, row != col)

    cor_plt <- cor_plt +
      ggplot2::geom_text(data = cell_data,
                         mapping = ggplot2::aes(x = row, y = col, label = round(cor, cell_label_digits)),
                         size = cell_label_size)
  }

  # Add row and column annotations
  if (is.data.frame(annot_rows_df)) {
    cor_plt <- add_annotation(cor_plt, annot_dim = "rows", annot_rows_df, annot_rows_pos, annot_rows_params$size,
                              annot_rows_params$border_lwd, annot_rows_params$border_col, annot_rows_params$legend, annot_rows_fill)
  }

  if (is.data.frame(annot_cols_df)) {
    cor_plt <- add_annotation(cor_plt, annot_dim = "cols", annot_cols_df, annot_cols_pos, annot_cols_params$size,
                              annot_cols_params$border_lwd, annot_cols_params$border_col, annot_cols_params$legend, annot_cols_fill)
  }

  # Add dendrograms
  if (lclust_data & dend_rows) {
    cor_plt <- add_dendrogram(cor_plt, dend_seg_rows, dend_rows_params$col, dend_rows_params$lwd, dend_rows_params$lty)
  }

  if (lclust_data & dend_cols) {
    cor_plt <- add_dendrogram(cor_plt, dend_seg_cols, dend_cols_params$col, dend_cols_params$lwd, dend_cols_params$lty)
  }

  if (return_data) {

    list_out <- list("plot" = cor_plt, "plot_data" = cor_long)

    if (cluster_data) {
      list_out$clustering <- clust
    }

    return(list_out)

  } else {
    return(cor_plt)
  }
}


