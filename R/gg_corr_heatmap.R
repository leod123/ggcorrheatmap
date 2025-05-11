#' Make a correlation heatmap with ggplot2.
#'
#' @param data A matrix or data frame containing numeric values, where the columns will be correlated with each other.
#' @param cor_method String specifying correlation method to use in the `cor` function. Default is 'pearson'.
#' @param cor_use String specifying the `use` argument of `cor`, which defineshow to deal with missing values. Default is 'everything'.
#' @param high Name of the colour to use for the highest value of the colour scale.
#' @param mid Name of the colour to use for 0 in the colour scale.
#' @param low Name of the colour to use for the lowest value of the colour scale.
#' @param limits Correlation limits to plot between.
#' @param layout String specifying the layout of the output correlation heatmap. Possible layouts include
#' top left, top right, bottom left, bottom right (default), or the whole heatmap. The string should be composed
#' of the vertical position (top or bottom) followed by the horizontal position (left or right). Bottom can
#' be specified by 'bottom', 'lower', 'down', or the first letter of these. Left is specified by 'left' or 'l'.
#' 'full', 'whole', or 'all' (or 'f', 'w', 'a') result in the whole correlation matrix being plotted.
#' For any other strings top and right are selected.
#' @param include_diag Logical indicating if the diagonal cells should be plotted (included either way if the whole matrix is plotted).
#' @param return_data Logical indicating if the data used for plotting (i.e. the correlation values) should be returned.
#' @param label_cells Logical specifying if the cells should be labelled with the correlation values.
#' @param cell_label_size Size of cell labels, used as the `size` argument in `ggplot2::geom_text`.
#' @param cell_label_digits Number of digits to display when cells are labelled with correlation coefficients. Default is 2, passed to `round`.
#' @param border_col Colour of cell borders.
#' @param border_lwd Size of cell borders, used for the `size` argument in `ggplot2::geom_tile`. Set to 0 to remove cell borders.
#' @param show_names Logical indicating if the names of the columns should be shown (displayed in the diagonal).
#' @param names_angle Angle to rotate the names by. Can be of length 1 to use for all names, or of the same length as the number
#' of columns in the data for column-specific angles.
#' @param names_size Size of the names. Can be of length 1 to use for all names, or of the same length as the number
#' of columns in the data for column-specific sizes
#' @param names_size_unit Units of the specified name size.
#' @param names_hjust Horizontal justification of names (between 0 and 1). Can be of length 1 to use for all names, or of the same length as the number
#' of columns in the data for column-specific justification.
#' @param names_vjust Verical justification of names (between 0 and 1). Can be of length 1 to use for all names, or of the same length as the number
#' of columns in the data for column-specific justification.
#' @param annot_df Data frame for row and column annotations. The names of the columns in the data must be included,
#' either as row names or in a column named `.names`. Each other column specifies an annotation where the column name
#' will be used as the annotation name (in the legend and next to the annotation). Numeric columns will use a continuous
#' colour scale while factor or character columns use discrete scales.
#' @param annot_rows Logical indicating if row annotation should be displayed.
#' @param annot_cols Logical indicating if column annotation should be displayed.
#' @param annot_colr not yet implemented (specifying colours for annotation)
#' @param annot_rows_colr not yet implemented
#' @param annot_cols_colr not yet implemented
#' @param annot_rows_side String specifying which side row annotation should be drawn ('left' for left, otherwise right).
#' @param annot_cols_side String specifying which side column annotation should be drawn ('bottom', 'down', 'lower' for bottom, otherwise top).
#' @param annot_dist Distance between heatmap and first annotation cell where 1 is the size of one heatmap cell. Used for both row and column annotation.
#' @param annot_rows_dist Used to overwrite `annot_dist` for row annotation.
#' @param annot_cols_dist Used to overwrite `annot_dist` for column annotation.
#' @param annot_gap Distance between each annotation where 1 is the size of one heatmap cell. Used for both row and column annotation.
#' @param annot_rows_gap Used to overwrite `annot_gap` for row annotation.
#' @param annot_cols_gap Used to overwrite `annot_gap` for column annotation.
#' @param annot_size Size (width for row annotation, height for column annotation) of annotation cells. Used for both row and column annotation.
#' @param annot_rows_size Used to overwrite `annot_size` for row annotation.
#' @param annot_cols_size Used to overwrite `annot_size` for column annotation.
#' @param annot_label not yet implemented (control if names of annotations should be shown in drawing area)
#' @param annot_rows_label not yet implemented
#' @param annot_cols_label not yet implemented
#' @param annot_label_size not yet implemented
#' @param annot_rows_label_size not yet implemented
#' @param annot_cols_label_size not yet implemented
#' @param annot_rows_border_col Colour of row annotation borders. By default uses the same colour as the heatmap cell borders.
#' @param annot_cols_border_col Colour of column annotation borders. By default uses the same colour as the row annotation cell borders.
#' @param annot_rows_border_lwd Line width of row annotation borders. By default uses the same value as the heatmap cell borders.
#' @param annot_cols_border_lwd Line width of column annotation borders. By default uses the same value as the row annotation cell borders.
#' @param clust_data Logical indicating if the correlations should be clustered in the heatmap. May also be
#' a `hclust` object where the correlations have been clustered already.
#' @param dist_method String with the distance metric to use for clustering, given to `dist`.
#' @param clust_method String with the clustering method to use, given to `hclust`.
#' @param dend_rows Logical indicating if a dendrogram should be drawn for the rows.
#' @param dend_cols Logical indicating if a dendrogram should be drawn for the columns.
#' @param dend_rows_height Number to scale the height of the row dendrogram by when plotting.
#' @param dend_cols_height Number to scale the height of the column dendrogram by when plotting.
#' @param dend_rows_lwd Line width of the row dendrogram.
#' @param dend_cols_lwd Line width of the column dendrogram.
#' @param dend_rows_side Which side to draw the row dendrogram on ('left' for left, otherwise right).
#' @param dend_cols_side Which side to draw the column dendrogram on ('bottom', 'down', 'lower' for bottom, otherwise top).
#' @param dend_rows_colour Colour of the row dendrogram.
#' @param dend_cols_colour Colour of the column dendrogram.
#' @param dend_options Options to give to `dendextend::set` for customising the dendrogram. Only works with options for colour, line type and line width.
#' The options are specified in a list where each element is a list corresponding to one call of
#' dendextend::set(). Each such list must either contain the 'what' argument as the first element or
#' as an element named 'what'. Other elements must be named and sepcify other parameters, such as
#' 'k' for 'branches_k_color' or 'value' used to specify colours.
#' @param legend_position Position of the legends (use how???)
#' @param plot_margin Plot margins, specified as a numeric vector of length 4 in the order of top, right, bottom, left.
#' @param margin_unit Unit to use for the specified margin.
#'
#' @return The correlation heatmap as a `ggplot` object.
#' If `return_data` is TRUE the output is a list containing the plot (named 'plot'),
#' the correlations ('plot_data'), and the result of the clustering ('clustering', only if `clust_data` is TRUE).
#' @export
#'
#' @examples
#' gg_corr_heatmap(mtcars)
gg_corr_heatmap <- function(data, cor_method = "pearson", cor_use = "everything",
                            high = "sienna2", mid = "white", low = "skyblue2", limits = c(-1, 1),
                            layout = "lowerright", include_diag = F, return_data = F,
                            label_cells = F, cell_label_size = 3, cell_label_digits = 2,
                            border_col = "grey", border_lwd = 0.5,
                            show_names = T, names_angle = 0, names_size = 3, names_size_unit = "mm",
                            names_hjust = 0.5, names_vjust = 0.5,
                            annot_df = NULL, annot_rows = T, annot_cols = T,
                            annot_colr = NULL, annot_rows_colr = annot_colr, annot_cols_colr = annot_colr,
                            annot_rows_side = "right", annot_cols_side = "bottom",
                            annot_dist = 0.2, annot_rows_dist = annot_dist, annot_cols_dist = annot_dist,
                            annot_gap = 0, annot_rows_gap = annot_gap, annot_cols_gap = annot_gap,
                            annot_size = 0.5, annot_rows_size = annot_size, annot_cols_size = annot_size,
                            annot_label = T, annot_rows_label = annot_label, annot_cols_label = annot_label,
                            annot_label_size = 4, annot_rows_label_size = annot_label_size, annot_cols_label_size = annot_label_size,
                            annot_rows_border_col = border_col, annot_cols_border_col = annot_rows_border_col,
                            annot_rows_border_lwd = border_lwd, annot_cols_border_lwd = annot_rows_border_lwd,
                            clust_data = T, dist_method = "euclidean", clust_method = "complete",
                            dend_rows = T, dend_cols = T,
                            dend_rows_height = 0.3, dend_cols_height = 0.3,
                            dend_rows_lwd = 0.3, dend_cols_lwd = 0.3,
                            dend_rows_side = "right", dend_cols_side = "bottom",
                            dend_rows_colour = "black", dend_cols_colour = "black",
                            dend_options = NULL,
                            legend_position = "right",
                            plot_margin = c(20, 10, 10, 20), margin_unit = "pt") {

  cor_mat <- cor(data, method = cor_method, use = cor_use)

  # Allow for providing a hclust object when clustering
  if (is.logical(clust_data)) {
    if (clust_data) {
      lclust_data <- T

      clust <- hclust(dist(cor_mat, method = dist_method), method = clust_method)

      dendro <- as.dendrogram(clust)

      # Apply dendextend options if any are given
      if (is.list(dend_options)) {
        # Go through options list and feed to the set() function with do.call()
        # Use append() to make named list of input arguments
        for (i in seq_along(dend_options)) {
          dendro <- do.call("set", append(list(dendro), dend_options[[i]]))
        }
      }

      dendro <- dendextend::as.ggdend(dendro)
      dendro$labels$label <- as.character(dendro$labels$label)

      cor_mat <- cor_mat[rev(dendro$labels$label), rev(dendro$labels$label)]

    } else {
      lclust_data <- F
    }


  } else if (class(clust_data) == "hclust") {
    lclust_data <- T
    clust <- clust_data
    dendro <- dendextend::as.ggdend(as.dendrogram(clust))
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
  if (is.data.frame(annot_df) & any(annot_rows, annot_cols)) {
    # Move names to column if in row names
    if (!".names" %in% colnames(annot_df)) {
      annot_df$.names <- rownames(annot_df)
      rownames(annot_df) <- NULL
    }

    annot_names <- colnames(annot_df)[-which(colnames(annot_df) == ".names")]

    # Calculate annotation positions
    if (annot_rows) {
      annot_rows_pos <- get_annotation_pos("rows", annot_left, annot_names, annot_rows_size,
                                           annot_rows_dist, annot_rows_gap, ncol(data))
    }

    if (annot_cols) {
      annot_cols_pos <- get_annotation_pos("cols", annot_cols, annot_names, annot_cols_size,
                                           annot_cols_dist, annot_cols_gap, ncol(data))
    }
  }

  # Generate dendrograms
  if (lclust_data & dend_rows) {

    dend_seg_rows <- prepare_dendrogram(dendro, "rows", dend_down, dend_left, dend_rows_height, full_plt, cor_long,
                                        annot_df, annot_rows, annot_left, annot_rows_pos, annot_rows_size)
  }

  if (lclust_data & dend_cols) {

    dend_seg_cols <- prepare_dendrogram(dendro, "cols", dend_down, dend_left, dend_cols_height, full_plt, cor_long,
                                        annot_df, annot_cols, annot_down, annot_cols_pos, annot_cols_size)
  }

  # Start building plot
  cor_plt <- ggplot2::ggplot(mapping = ggplot2::aes(x = row, y = col))
  # Draw diagonal first to draw over with the rest of the plot
  if (include_diag) {
    cor_plt <- cor_plt +
      ggplot2::geom_tile(ggplot2::aes(fill = cor), subset(cor_long, row == col),
                         linewidth = border_lwd, colour = border_col)
  } else {
    cor_plt <- cor_plt +
      ggplot2::geom_tile(data = subset(cor_long, row == col), fill = "white", linewidth = 0)
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
    ggplot2::geom_tile(ggplot2::aes(fill = cor), subset(cor_long, row != col),
                       linewidth = border_lwd, colour = border_col) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0), position = "right") +
    ggplot2::scale_fill_gradient2(limits = limits, high = high, mid = mid, low = low,
                                  guide = ggplot2::guide_colourbar(order = 1)) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::labs(fill = switch(cor_method,
                                "pearson" = "Pearson r",
                                "spearman" = "Spearman\nrho",
                                "kendall" = "Kendall\ntau")) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   legend.position = legend_position, legend.position.inside = legend_position_inside,
                   plot.margin = ggplot2::margin(plot_margin[1], plot_margin[2], plot_margin[3], plot_margin[4], margin_unit))

  if (show_names) {
    axis_lab <- data.frame(lab = levels(cor_long$row))

    cor_plt <- cor_plt +
      ggplot2::geom_text(data = axis_lab, mapping = ggplot2::aes(x = lab, y = lab, label = lab),
                         size = names_size, size.unit = names_size_unit,
                         angle = names_angle, hjust = names_hjust, vjust = names_vjust)

  }

  if (label_cells) {
    cor_plt <- cor_plt +
      ggplot2::geom_text(data = subset(cor_long, row != col),
                         mapping = ggplot2::aes(x = row, y = col, label = round(cor, cell_label_digits)),
                         size = cell_label_size)
  }

  # Add row and column annotations
  annot_lgd <- T
  if (is.data.frame(annot_df) & annot_rows) {
    cor_plt <- add_annotation(cor_plt, annot_dim = "rows", annot_df, annot_rows_pos, annot_rows_size,
                              annot_rows_border_lwd, annot_rows_border_col, annot_lgd)
    # Only draw the legend once
    annot_lgd <- F
  }

  if (is.data.frame(annot_df) & annot_cols) {
    cor_plt <- add_annotation(cor_plt, annot_dim = "cols", annot_df, annot_cols_pos, annot_cols_size,
                              annot_cols_border_lwd, annot_cols_border_col, annot_lgd)
  }

  if (lclust_data & dend_rows) {
    if (all(is.na(dend_seg_rows$col))) {
      cor_plt <- cor_plt +
        ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend), dend_seg_rows,
                              colour = dend_rows_colour,
                              linewidth = if (all(is.na(dend_seg_rows$lwd))) {dend_rows_lwd}
                              else {dend_seg_rows$lwd},
                              linetype = if (all(is.na(dend_seg_rows$lty))) {1}
                              else {dend_seg_rows$lty})
    } else {
      seg_colr <- pull(distinct(dend_seg_rows, col), col)
      names(seg_colr) <- seg_colr
      cor_plt <- cor_plt +
        ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, colour = col), dend_seg_rows,
                              linewidth = if (all(is.na(dend_seg_rows$lwd))) {dend_rows_lwd}
                              else {dend_seg_rows$lwd},
                              linetype = if (all(is.na(dend_seg_rows$lty))) {1}
                              else {dend_seg_rows$lty},
                              show.legend = F) +
        ggplot2::scale_colour_manual(values = seg_colr)
    }

  }

  if (lclust_data & dend_cols) {
    if (all(is.na(dend_seg_cols$col))) {
      cor_plt <- cor_plt +
        ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend), dend_seg_cols,
                              colour = dend_cols_colour,
                              linewidth = if (all(is.na(dend_seg_cols$lwd))) {dend_cols_lwd}
                              else {dend_seg_cols$lwd},
                              linetype = if (all(is.na(dend_seg_cols$lty))) {1}
                              else {dend_seg_cols$lty})
    } else {
      cor_plt <- cor_plt +
        ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, colour = col), dend_seg_cols,
                              linewidth = if (all(is.na(dend_seg_cols$lwd))) {dend_cols_lwd}
                              else {dend_seg_cols$lwd},
                              linetype = if (all(is.na(dend_seg_cols$lty))) {1}
                              else {dend_seg_cols$lty},
                              show.legend = F)
      if (lclust_data & !dend_cols) {
        # Add colour scale only if row dendrogram is skipped, otherwise a message is displayed
        # about existing colour scales
        seg_colr <- pull(distinct(dend_seg_cols, col), col)
        names(seg_colr) <- seg_colr
        cor_plt <- cor_plt + ggplot2::scale_colour_manual(values = seg_colr)
      }
    }
  }

  if (return_data) {

    list_out <- list("plot" = cor_plt, "plot_data" = cor_long)

    if (clust_data) {
      list_out$clustering <- clust
    }

    return(list_out)

  } else {
    return(cor_plt)
  }
}


