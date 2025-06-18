make_heatmap <- function(x_long, plt = NULL, layout = "f", include_diag = F, invisible_diag = F,
                         cell_shape = NULL, border_lwd = 0.5, border_col = "grey", border_lty = 1,
                         names_diag = T, names_x = F, names_y = F, names_diag_param = NULL,
                         names_x_side = "top", names_y_side = "left", show_legend = T,
                         fill_scale = NULL, fill_name = "value", size_scale = NULL) {
  # Base plot
  plt_provided <- !is.null(plt)
  if (is.null(plt)) {
    plt <- ggplot2::ggplot(mapping = ggplot2::aes(x = col, y = row))
  }

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
    # Plot tiles or points depending on cell_shape
    list(
      if (is.numeric(cell_shape)) {
        ggplot2::geom_point(ggplot2::aes(fill = value, size = value),
                            data = x_plot_dat,
                            stroke = border_lwd, colour = border_col, shape = cell_shape,
                            show.legend = show_legend)
      } else {
        ggplot2::geom_tile(ggplot2::aes(fill = value),
                           data = x_plot_dat,
                           linewidth = border_lwd, colour = border_col, linetype = border_lty,
                           show.legend = show_legend)
      }
    ) +
    size_scale +
    fill_scale +
    ggplot2::labs(fill = fill_name)

  # Only add scales and coordinate systems once to avoid messages
  if (!plt_provided) {
    # Remove extra space on axes (if drawing tiles) and place on specified sides
    plt <- plt +
      ggplot2::scale_x_discrete(expand = if (is.numeric(cell_shape)) c(.05, .05) else c(0, 0), position = names_x_side) +
      ggplot2::scale_y_discrete(expand = if (is.numeric(cell_shape)) c(.05, .05) else c(0, 0), position = names_y_side) +
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
    axis_lab <- data.frame(lab = levels(x_long$row))

    # Construct call using optional parameters
    text_call_params <- list(data = axis_lab, mapping = ggplot2::aes(x = lab, y = lab, label = lab))
    if (is.list(names_diag_param)) {
      text_call_params <- append(text_call_params, names_diag_param)
    }

    plt <- plt +
      do.call(ggplot2::geom_text, text_call_params)
  }

  return(plt)
}
