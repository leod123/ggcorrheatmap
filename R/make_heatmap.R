#' Make main heatmap part of plot for gghm.
#'
#' @keywords internal
#'
#' @param x_long Long format data.
#' @param plt A ggplot object to build onto. If NULL, makes a new plot.
#' @param mode Plotting mode.
#' @param include_diag Logical indicating if diagonal should be included.
#' @param invisible_diag Logical indicating if an invisible diagonal should be included.
#' @param border_lwd Border linewidth.
#' @param border_col Border colour.
#' @param border_lty Border linetype.
#' @param names_diag Logical indicating if names should be displayed on the diagonal.
#' @param names_x Logical indicating if names should be displayed on the x axis.
#' @param names_y Logical indicating if names should be displayed on the y axis.
#' @param names_diag_param Parameters for diagonal names.
#' @param names_x_side X axis side.
#' @param names_y_side Y axis side.
#' @param show_legend Logical indicating which, if any, legends should be drawn.
#' @param fill_scale Scale for fill aesthetic.
#' @param fill_name Name for fill aesthetic.
#' @param col_scale Scale for colour aesthetic.
#' @param col_name Name for colour aesthetic.
#' @param size_scale Scale for size aesthetic.
#' @param cell_labels Logical indicating if cell labels should be written.
#' @param cell_label_col Colour of cell labels.
#' @param cell_label_size Size of cell labels.
#' @param cell_label_digits Number of digits for cell labels if numeric.
#'
#' @returns ggplot object with heatmap component.
#'
make_heatmap <- function(x_long, plt = NULL, mode = "heatmap",
                         include_diag = F, invisible_diag = F,
                         border_lwd = 0.5, border_col = "grey", border_lty = 1,
                         names_diag = T, names_x = F, names_y = F, names_diag_param = NULL,
                         names_x_side = "top", names_y_side = "left", show_legend = T,
                         fill_scale = NULL, fill_name = "value", col_scale = NULL, col_name = fill_name,
                         size_scale = NULL, cell_labels = F, cell_label_col = "black", cell_label_size = 3, cell_label_digits = 2) {
  value <- .data <- lab <- NULL

  # Base plot
  plt_provided <- !is.null(plt)
  if (is.null(plt)) {
    plt <- ggplot2::ggplot(mapping = ggplot2::aes(x = col, y = row))
  }

  shape_mode_fill <- mode %in% as.character(21:25)
  shape_mode_col <- mode %in% as.character(1:20)

  # Draw diagonal invisibly to reserve space for it, making it easier to place the labels
  # (only needed if symmetric matrix with triangular layout and hidden diagonal)
  if (invisible_diag) {
    plt <- plt +
      # Subset after converting to character as error occurs during rendering if factor levels are different
      ggplot2::geom_tile(data = subset(x_long, as.character(row) == as.character(col)),
                         fill = "white", linewidth = 0, alpha = 0)
  }

  # Use different input data depending on desired layout
  # If include_diag is FALSE, skip where row == col, otherwise use the whole data
  x_plot_dat <- if (!include_diag) {
    subset(x_long, as.character(row) != as.character(col))
  } else {
    x_long
  }

  plt <- plt +
    # Plot tiles or points depending on cell shape
    list(
      if (mode %in% c("heatmap", "hm")) {
        ggplot2::geom_tile(ggplot2::aes(fill = value),
                           data = x_plot_dat,
                           linewidth = border_lwd, colour = border_col, linetype = border_lty,
                           show.legend = show_legend)
      } else if (shape_mode_fill) {
        ggplot2::geom_point(ggplot2::aes(fill = value, size = value),
                            data = x_plot_dat,
                            stroke = border_lwd, colour = border_col, shape = as.numeric(mode),
                            show.legend = show_legend)
      } else if (shape_mode_col) {
        ggplot2::geom_point(ggplot2::aes(colour = value, size = value),
                            data = x_plot_dat,
                            stroke = border_lwd, shape = as.numeric(mode),
                            show.legend = show_legend)
      } else if (mode == "text") {
        ggplot2::geom_text(
          # Round values if value are numeric and cell_label_digits is numeric
          ggplot2::aes(label = if (is.numeric(.data[["value"]]) & is.numeric(cell_label_digits)) {
            round(value, cell_label_digits)
          } else {value}, colour = value),
          # For text, skip diagonals if names are written there
          data = if (names_diag) {
            subset(x_plot_dat, as.character(row) != as.character(col))
          } else {x_plot_dat},
          size = cell_label_size,
          show.legend = show_legend
        )
      } else if (mode == "none") {
        # Draw nothing, mostly for internal use to mimic text mode with ggcorrhm
      }
    ) +
    # Add empty cells as a grid for text and none modes
    list(
      if (mode %in% c("text", "none")) {
        ggplot2::geom_tile(data = x_plot_dat, linewidth = border_lwd, colour = border_col,
                           linetype = border_lty, alpha = 0)
      }
    ) +
    fill_scale + col_scale + size_scale +
    ggplot2::labs(fill = fill_name, colour = col_name)

  # Only add scales and coordinate systems once to avoid messages
  if (!plt_provided) {
    plt <- plt +
      # Remove extra space on axes (if drawing tiles) and place on specified sides
      ggplot2::scale_x_discrete(position = names_x_side) +
      ggplot2::scale_y_discrete(position = names_y_side) +
      # Make cells square
      ggplot2::coord_fixed(clip = "off") +
      ggplot2::theme_classic() +
      # Remove axis elements
      ggplot2::theme(axis.line = ggplot2::element_blank(),
                     axis.text.y = if (names_y) ggplot2::element_text() else ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank())
    # Rotate x labels if names on x axis
    if (names_x) {
      plt <- plt +
        if (names_x_side == "top") ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.3))
      else if (names_x_side == "bottom") ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.3))
    } else {
      plt <- plt + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }
  }

  # Names on the diagonal
  if (names_diag) {
    # Order labels so they are from left to right
    axis_lab <- data.frame(lab = factor(levels(x_long$col), levels = levels(x_long$col)))
    axis_lab <- dplyr::arrange(axis_lab, lab)

    # Construct call using optional parameters
    text_call_params <- list(data = axis_lab, mapping = ggplot2::aes(x = lab, y = lab, label = lab))
    if (is.list(names_diag_param)) {
      text_call_params <- append(text_call_params, names_diag_param)
    }

    plt <- plt +
      do.call(ggplot2::geom_text, text_call_params)
  }

  # Cell labels
  if (cell_labels) {
    # Don't draw in the diagonal cells if are hidden or if they are already occupied by names
    cell_data <- if (include_diag & !names_diag) x_long else subset(x_long, as.character(row) != as.character(col))

    plt <- plt +
          ggplot2::geom_text(data = cell_data, size = cell_label_size, colour = cell_label_col,
                             mapping = ggplot2::aes(x = col, y = row,
                                                    label = if (is.numeric(value) & is.numeric(cell_label_digits)) {
                                                      round(value, cell_label_digits)
                                                    } else {value}))
  }

  return(plt)
}
