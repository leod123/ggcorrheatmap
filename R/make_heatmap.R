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
#' @param show_names_diag Logical indicating if names should be displayed on the diagonal.
#' @param show_names_x Logical indicating if names should be displayed on the x axis.
#' @param show_names_y Logical indicating if names should be displayed on the y axis.
#' @param names_x_side X axis side.
#' @param names_y_side Y axis side.
#' @param col_scale Scale for colour/fill aesthetic.
#' @param size_scale Scale for size aesthetic.
#' @param cell_labels Data frame of text to write on cells (processed by check_cell_labels).
#' @param cell_label_col Colour of cell labels.
#' @param cell_label_size Size of cell labels.
#' @param cell_label_digits Number of digits for cell labels if numeric.
#' @param cell_bg_col Cell background colour (fill).
#' @param cell_bg_alpha Cell background alpha.
#' @param split_rows_names,split_cols_names Logicals indicating if the facet names should be shown (if plot is built from scratch).
#' @param split_rows_side,split_cols_side Sides to put the facet strips.
#'
#' @returns ggplot object with heatmap component.
#'
make_heatmap <- function(x_long, plt = NULL, mode = "heatmap",
                         include_diag = TRUE, invisible_diag = FALSE,
                         border_lwd = 0.1, border_col = "grey", border_lty = 1,
                         show_names_diag = TRUE, show_names_x = FALSE, show_names_y = FALSE,
                         names_x_side = "top", names_y_side = "left",
                         col_scale = NULL, size_scale = NULL,
                         cell_labels = FALSE, cell_label_col = "black",
                         cell_label_size = 3, cell_label_digits = 2,
                         cell_bg_col = "white", cell_bg_alpha = 0,
                         split_rows_names = FALSE, split_cols_names = FALSE,
                         split_rows_side = "right", split_cols_side = "bottom") {
  value <- .data <- label <- .row_facets <- .col_facets <- NULL

  # show_names_diag checked earlier
  check_logical(show_names_x = show_names_x)
  check_logical(show_names_y = show_names_y)
  check_logical(include_diag = include_diag)

  # Base plot
  plt_provided <- !is.null(plt)
  if (is.null(plt)) {
    plt <- ggplot2::ggplot()
  }

  shape_mode_fill <- mode %in% as.character(21:25)
  shape_mode_col <- mode %in% as.character(1:20)

  # Draw diagonal invisibly to reserve space for it, making it easier to place the labels
  # (only needed if symmetric matrix with triangular layout)
  if (invisible_diag) {
    plt <- plt +
      # Subset after converting to character as error occurs during rendering if factor levels are different
      ggplot2::geom_tile(data = subset(x_long, as.character(row) == as.character(col)),
                         mapping = ggplot2::aes(x = col, y = row),
                         fill = "white", linewidth = 0, alpha = 0)
  }

  # Use different input data depending on desired layout
  # If include_diag is FALSE, skip where row == col, otherwise use the whole data
  x_long <- if (isFALSE(include_diag)) {
    subset(x_long, as.character(row) != as.character(col))
  } else if (isTRUE(include_diag)) {
    x_long
  }

  plt <- plt +
    # Add empty cells as a grid for text and none modes
    list(
      if (mode %in% c("text", "none")) {
        ggplot2::geom_tile(data = x_long, mapping = ggplot2::aes(x = col, y = row),
                           linewidth = border_lwd, colour = border_col,
                           fill = cell_bg_col, linetype = border_lty, alpha = cell_bg_alpha)
      }
    ) +
    # Plot tiles or points depending on cell shape
    list(
      if (mode %in% c("heatmap", "hm")) {
        ggplot2::geom_tile(ggplot2::aes(x = col, y = row, fill = value),
                           data = x_long,
                           linewidth = border_lwd, colour = border_col, linetype = border_lty)
      } else if (shape_mode_fill) {
        ggplot2::geom_point(ggplot2::aes(x = col, y = row, fill = value, size = value),
                            data = x_long,
                            stroke = border_lwd, colour = border_col, shape = as.numeric(mode))
      } else if (shape_mode_col) {
        ggplot2::geom_point(ggplot2::aes(x = col, y = row, colour = value, size = value),
                            data = x_long,
                            stroke = border_lwd, shape = as.numeric(mode))
      } else if (mode == "text" & isFALSE(cell_labels)) {
        ggplot2::geom_text(
          # Round values if value are numeric and cell_label_digits is numeric
          ggplot2::aes(x = col, y = row, label = if (is.numeric(.data[["value"]]) & is.numeric(cell_label_digits)) {
            round(value, cell_label_digits)
          } else {value}, colour = value),
          # For text, skip diagonals if names are written there
          data = if (show_names_diag) {
            subset(x_long, as.character(row) != as.character(col))
          } else {x_long},
          size = cell_label_size
        )
      } else if (mode == "none") {
        # Draw nothing
      }
    ) +
    col_scale + size_scale

  # Only add scales and coordinate systems once to avoid messages
  if (!plt_provided) {
    plt <- plt +
      # Remove extra space on axes (if drawing tiles) and place on specified sides
      ggplot2::scale_x_discrete(position = names_x_side) +
      ggplot2::scale_y_discrete(position = names_y_side) +
      ggplot2::theme_classic() +
      # Remove axis elements
      ggplot2::theme(axis.line = ggplot2::element_blank(),
                     axis.text.y = if (show_names_y) ggplot2::element_text() else ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank())
    # Rotate x labels if names on x axis
    if (show_names_x) {
      plt <- plt +
        if (names_x_side == "top") ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.3))
      else if (names_x_side == "bottom") ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.3))
    } else {
      plt <- plt + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }
  }

  # Cell labels
  if (is.data.frame(cell_labels)) {
    # Remove cells in cell_labels that don't exist in plot
    # If input is a matrix, convert to long format

    # Merge with input long matrix to throw away irrelevant rows
    cell_data <- dplyr::left_join(x_long, cell_labels, by = c("row", "col"))

    # Skip NA labels
    cell_data <- dplyr::select(subset(cell_data, !is.na(label)), "row", "col", "value", "label",
                               dplyr::contains("_facets"))

    # skip diagonal if already occupied
    if (!(include_diag & !show_names_diag)) {cell_data <- subset(cell_data, as.character(row) != as.character(col))}

    plt <- plt +
      list(
        if (mode != "text") {
          # If not text mode, use cell_label_col for colours
          ggplot2::geom_text(data = cell_data, size = cell_label_size, colour = cell_label_col,
                             mapping = ggplot2::aes(x = col, y = row,
                                                    label = if (is.numeric(label) & is.numeric(cell_label_digits)) {
                                                      round(label, cell_label_digits)
                                                    } else {label}))
        } else {
          # If text mode, scale colours with cell values
          ggplot2::geom_text(data = cell_data, size = cell_label_size,
                             mapping = ggplot2::aes(x = col, y = row, colour = value,
                                                    label = if (is.numeric(label) & is.numeric(cell_label_digits)) {
                                                      round(label, cell_label_digits)
                                                    } else {label}))
        }
      )

  }

  if (any(c(".row_facets", ".col_facets") %in% colnames(x_long))) {
    facet_r <- if (".row_facets" %in% colnames(x_long)) {
      # Explicitly set factor levels when making the facets to prevent unintentional changes in facet order
      ggplot2::vars(factor(.row_facets, levels = levels(x_long[[".row_facets"]])))
    } else {NULL}
    facet_c <- if (".col_facets" %in% colnames(x_long)) {
      ggplot2::vars(factor(.col_facets, levels = levels(x_long[[".col_facets"]])))
    } else {NULL}

    # Make input for the 'switch' argument for strip placement
    # Check that the inputs are valid
    if (!split_rows_side %in% c("left", "right") && !is.null(facet_r)) {
      cli::cli_warn("{.var split_rows_side} should be {.val left} or {.val right}, not
                     {.val {split_rows_side}}. Using default (right).",
                     class = "facet_side_warn")
      split_rows_side <- "right"
    }
    if (!split_cols_side %in% c("top", "bottom") && !is.null(facet_c)) {
      cli::cli_warn("{.var split_cols_side} should be {.val top} or {.val bottom}, not
                     {.val {split_cols_side}}. Using default (bottom).",
                     class = "facet_side_warn")
      split_cols_side <- "bottom"
    }

    # Default is top and right, switch different axes depending on input
    sw_in <- switch(paste0(split_rows_side, split_cols_side),
                    "righttop" = NULL,
                    "rightbottom" = "x",
                    "lefttop" = "y",
                    "leftbottom" = "both")

    plt <- plt +
      ggplot2::facet_grid(rows = facet_r, cols = facet_c,
                          space = "free", scales = "free",
                          switch = sw_in) +
      ggplot2::theme(strip.background = ggplot2::element_blank())
  }

  if (!plt_provided && !any(c(".row_facets", ".col_facets") %in% colnames(x_long))) {
    plt <- plt +
      # Make cells square
      ggplot2::coord_fixed(clip = "off")
  } else if (!plt_provided) {
    # coord_fixed does not work with facets and free scales
    plt <- plt +
      # Keep normal coordinates but turn of clipping
      ggplot2::coord_cartesian(clip = "off")
  }

  # Hide facet label strips if facet input argument is shorter than number of rows/cols
  if (!plt_provided && isFALSE(split_rows_names)) {
    plt <- plt + ggplot2::theme(strip.background.y = ggplot2::element_blank(),
                                strip.text.y.left = ggplot2::element_blank(),
                                strip.text.y.right = ggplot2::element_blank())
  }
  if (!plt_provided && isFALSE(split_cols_names)) {
    plt <- plt + ggplot2::theme(strip.background.x = ggplot2::element_blank(),
                                strip.text.x.bottom = ggplot2::element_blank(),
                                strip.text.x.top = ggplot2::element_blank())
  }

  return(plt)
}


#' Add diagonal names to heatmap.
#'
#' @keywords internal
#'
#' @param plt Plot object to add names to.
#' @param x_long Long format plotting data.
#' @param names_diag_params Parameters for diagonal names.
#'
#' @returns Plot with labels added.
#'
add_diag_names <- function(plt, x_long, names_diag_params = NULL) {
  label <- NULL

  # Names on the diagonal
  # Order labels so they are from left to right
  axis_lab <- data.frame(label = factor(levels(x_long$col), levels = levels(x_long$col)))
  axis_lab <- dplyr::arrange(axis_lab, label)

  # Add facetting columns
  if (".row_facets" %in% colnames(x_long)) {
    axis_lab <- dplyr::left_join(axis_lab,
                                 dplyr::distinct(dplyr::select(x_long, "row", ".row_facets")),
                                 by = c("label" = "row"))
  }
  if (".col_facets" %in% colnames(x_long)) {
    axis_lab <- dplyr::left_join(axis_lab,
                                 dplyr::distinct(dplyr::select(x_long, "col", ".col_facets")),
                                 by = c("label" = "col"))
  }

  # Construct call using optional parameters
  text_call_params <- list(data = axis_lab, mapping = ggplot2::aes(x = label, y = label, label = label))
  if (is.list(names_diag_params)) {
    text_call_params <- append(text_call_params, names_diag_params)
  } else if (!is.null(names_diag_params)) {
    cli::cli_warn("{.var names_diag_params} should be a list with static aesthetics to
                  pass to {.var ggplot2::geom_text}.",
                  class = "diag_names_arg_warn")
  }

  plt <- plt +
    do.call(ggplot2::geom_text, text_call_params)

  return(plt)
}
