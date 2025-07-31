#' `gghm()` for long format data using 'tidy' input.
#'
#' @param x Data frame containing data to plot.
#' @param rows Name of column to use as rows.
#' @param cols Name of column to use as columns.
#' @param values Name of column to use for cell values.
#' @param labels Name of column to use for cell labels. NULL (default) for no labels.
#' @param annot_rows Names of columns to use for row annotations.
#' @param annot_cols Names of columns to use for column annotations.
#' @param ... Additional arguments for `gghm()`.
#'
#' @export
#'
#' @return A ggplot2 object with the heatmap. If `return_data` is `TRUE`, plotting data is returned as well.
#'
#' @examples
#' # Basic example
#' set.seed(123)
#' hm_in <- data.frame(row = rep(letters[1:10], each = 5),
#'                     col = rep(LETTERS[1:5], 10),
#'                     val = rnorm(50))
#' gghm_tidy(hm_in, row, col, val)
#'
#' # Annotation and clustering
#' # Add annotation by giving names of columns in the data
#' hm_in$row_annot1 <- rep(1:10, each = 5)
#' hm_in$row_annot2 <- rep(10:1, each = 5)
#' hm_in$col_annot <- rep(letters[1:5], 10)
#' gghm_tidy(hm_in, row, col, val,
#'           annot_rows = c(row_annot1, row_annot2),
#'           annot_cols = col_annot,
#'           cluster_rows = TRUE,
#'           cluster_cols = TRUE)
#'
#' # Add cell labels
#' hm_in$lab <- 1:50
#' gghm_tidy(hm_in, row, col, val,
#'           labels = lab, cell_label_col = "white")
#'
gghm_tidy <- function(x, rows, cols, values, labels = NULL, annot_rows = NULL, annot_cols = NULL, ...) {

  if (missing(x)) cli::cli_abort("Argument {.var x} is missing. It needs to be a data frame.")
  if (missing(rows)) cli::cli_abort("Argument {.var rows} is missing. Provide the name of the column that contains the heatmap rownames.")
  if (missing(cols)) cli::cli_abort("Argument {.var cols} is missing. Provide the name of the column that contains the heatmap colnames.")
  if (missing(values)) cli::cli_abort("Argument {.var values} is missing. Provide the name of the column that contains the heatmap values.")

  if (!is.data.frame(x)) {
    cli::cli_abort("{.var x} must be a {.cls data.frame}, not a {.cls {class(x)}}.",
                   class = "tidy_input_class_error")
  }

  # Move this to a separate function and reuse in ggcorrhm_long
  x_long <- dplyr::select(x, {{rows}}, {{cols}}, {{values}})

  # Check that rows and cols are not too many
  if (ncol(x_long) > 3) {
    cli::cli_abort("Too many columns, provide one column each for {.var rows}, {.var cols} and {.var values}.",
                   class = "tidy_too_many_cols_error")
  }

  # Convert to wide format
  colnames(x_long)[1:3] <- c("row", "col", "value")
  x_long <- as.data.frame(x_long)
  x_wide <- shape_mat_wide(x_long)

  # Reorder rows and columns if input data has factor levels
  if (is.factor(x_long[["row"]])) {
    x_wide <- x_wide[levels(x_long[["row"]]), ]
  }
  if (is.factor(x_long[["col"]])) {
    x_wide <- x_wide[, levels(x_long[["col"]])]
  }

  # Cell labels
  if (!rlang::quo_is_null(rlang::enquo(labels))) {
    cell_label_df <- dplyr::select(x, {{rows}}, {{cols}}, {{labels}})
    colnames(cell_label_df) <- c("row", "col", "value")
    cell_label_df <- as.data.frame(cell_label_df)
    cell_label_df <- shape_mat_wide(cell_label_df)

  } else {
    cell_label_df <- FALSE
  }

  # Annotation
  # Make the annotation data frames if any columns have been provided
  if (!rlang::quo_is_null(rlang::enquo(annot_rows))) {
    annot_rows_df <- dplyr::distinct(dplyr::select(x, {{rows}}, {{annot_rows}}))
    colnames(annot_rows_df)[1] <- ".names"

  } else {
    annot_rows_df <- NULL
  }

  if (!rlang::quo_is_null(rlang::enquo(annot_cols))) {
    annot_cols_df <- dplyr::distinct(dplyr::select(x, {{cols}}, {{annot_cols}}))
    colnames(annot_cols_df)[1] <- ".names"
  } else {
    annot_cols_df <- NULL
  }

  plt <- gghm(x_wide,
              cell_labels = cell_label_df,
              annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df,
              ...)

  return(plt)
}

#' `ggcorrhm()` for long format data using 'tidy' input.
#'
#' @param x Data containing data to plot or to correlate.
#' @param rows Name of column to use as rows in the plotted matrix (if `cor_in` is TRUE) or the matrix to compute correlations from (`cor_in` is FALSE).
#' @param cols Name of column to use as columns in the plotted matrix (if `cor_in` is TRUE) or the matrix to compute correlations from (`cor_in` is FALSE).
#' @param values Name of column to use as correlation values (if `cor_in` is TRUE) or values in the matrix to compute correlations from (`cor_in` is FALSE).
#' @param annot_rows Names of columns containing values for row annotations.
#' @param annot_cols Names of columns containing values for column annotations.
#' @param labels Name of column to use for cell labels, NULL for no labels, or TRUE to use the cell values. If `cor_in` is FALSE, only NULL, TRUE or FALSE is supported.
#' @param cor_in Logical indicating if the values are correlation values (TRUE, default) or values to be correlated. See details for more information.
#' @param ... Additional arguments for `ggcorrhm()`.
#'
#' @details
#' If `cor_in` is TRUE (the default), `ggcorrhm_tidy()` behaves similarly to `gghm_tidy()` but with the colour
#' scales and arguments of `ggcorrhm()` instead of `gghm()`.
#'
#' If `cor_in` FALSE, the data is converted to wide format and the column-column correlations are computed.
#' This means that if asymmetric correlation matrices are to be plotted the correlations have to be computed
#' in advance and plotted with `cor_in` as TRUE. Additionally, `annot_rows` and `annot_cols` will both use
#' the `cols` column for names, and `labels` can only take TRUE or FALSE.
#'
#' @returns A ggplot2 object with the heatmap. If `return_data` is `TRUE`, plotting data is returned as well.
#' @export
#'
#' @examples
#' library(dplyr)
#' # Basic example with long format correlation data
#' # Make some correlation data in long format
#' cor_dat <- cor(mtcars)
#' hm_in <- data.frame(row = rep(colnames(cor_dat), ncol(cor_dat)),
#'                     col = rep(colnames(cor_dat), each = ncol(cor_dat)),
#'                     val = as.vector(cor_dat))
#'
#' ggcorrhm_tidy(hm_in, row, col, val,
#'               # Indicate that the data consists of correlation coefficients
#'               cor_in = TRUE)
#'
#' # Or let the function compute the correlations
#' # (this limits some other functionality, see details)
#' raw_dat <- data.frame(row = rep(rownames(mtcars), ncol(mtcars)),
#'                       col = rep(colnames(mtcars), each = nrow(mtcars)),
#'                       val = unlist(mtcars))
#' ggcorrhm_tidy(raw_dat, row, col, val, cor_in = FALSE)
#'
ggcorrhm_tidy <- function(x, rows, cols, values, annot_rows = NULL, annot_cols = NULL,
                          labels = NULL, cor_in = TRUE, ...) {

  if (missing(x)) cli::cli_abort("Argument {.var x} is missing. It needs to be a data frame.")
  if (missing(rows)) cli::cli_abort("Argument {.var rows} is missing. Provide the name of the column that contains the heatmap rownames.")
  if (missing(cols)) cli::cli_abort("Argument {.var cols} is missing. Provide the name of the column that contains the heatmap colnames.")
  if (missing(values)) cli::cli_abort("Argument {.var values} is missing. Provide the name of the column that contains the heatmap values.")

  if (!is.data.frame(x)) {
    cli::cli_abort("{.var x} must be a {.cls data.frame}, not a {.cls {class(x)}}.",
                   class = "tidy_input_class_error")
  }

  # Move this to a separate function and reuse in ggcorrhm_long
  x_long <- dplyr::select(x, {{rows}}, {{cols}}, {{values}})

  # Check that rows and cols are not too many
  if (ncol(x_long) > 3) {
    cli::cli_abort("Too many columns, provide one column each for {.var rows}, {.var cols} and {.var values}.",
                   class = "tidy_too_many_cols_error")
  }

  # Convert to wide format
  colnames(x_long)[1:3] <- c("row", "col", "value")
  x_long <- as.data.frame(x_long)
  x_wide <- shape_mat_wide(x_long)

  # Behave like gghm_tidy if cor_in is TRUE (but use ggcorrhm for the correlation heatmap utilities)
  check_logical(cor_in = cor_in)
  if (isTRUE(cor_in)) {
    if (!rlang::quo_is_null(rlang::enquo(labels))) {
      cell_label_df <- dplyr::select(x, {{rows}}, {{cols}}, {{labels}})
      colnames(cell_label_df) <- c("row", "col", "value")
      cell_label_df <- as.data.frame(cell_label_df)
      cell_label_df <- shape_mat_wide(cell_label_df)

    } else {
      cell_label_df <- FALSE
    }

    if (!rlang::quo_is_null(rlang::enquo(annot_rows))) {
      annot_rows_df <- dplyr::distinct(dplyr::select(x, {{rows}}, {{annot_rows}}))
      colnames(annot_rows_df)[1] <- ".names"

    } else {
      annot_rows_df <- NULL
    }

    if (!rlang::quo_is_null(rlang::enquo(annot_cols))) {
      annot_cols_df <- dplyr::distinct(dplyr::select(x, {{cols}}, {{annot_cols}}))
      colnames(annot_cols_df)[1] <- ".names"
    } else {
      annot_cols_df <- NULL
    }

    # Reorder rows and columns if input data has factor levels
    if (is.factor(x_long[["row"]])) {
      x_wide <- x_wide[levels(x_long[["row"]]), ]
    }
    if (is.factor(x_long[["col"]])) {
      x_wide <- x_wide[, levels(x_long[["col"]])]
    }

  } else {
    # If cor_in is FALSE, labels can only take TRUE or FALSE
    # and annotation only takes the columns into consideration
    cell_label_df <- if (is.null(labels) || isFALSE(labels)) {
      FALSE
    } else {
      TRUE
    }

    if (!is.logical(labels) && !is.null(labels)) {
      cli::cli_warn("If {.var cor_in} is FALSE, {.var labels} must be TRUE, FALSE, or NULL.
                    Anything else will write the cell values.",
                    class = "tidy_corr_labels")
    }

    if (!rlang::quo_is_null(rlang::enquo(annot_rows))) {

      # Unlike gghm_long, use cols for both rows and cols
      annot_rows_df <- dplyr::distinct(dplyr::select(x, {{cols}}, {{annot_rows}}))
      colnames(annot_rows_df)[1] <- ".names"

    } else {
      annot_rows_df <- NULL
    }

    if (!rlang::quo_is_null(rlang::enquo(annot_cols))) {
      annot_cols_df <- dplyr::distinct(dplyr::select(x, {{cols}}, {{annot_cols}}))
      colnames(annot_cols_df)[1] <- ".names"
    } else {
      annot_cols_df <- NULL
    }

    if (is.factor(x_long[["row"]])) {
      x_wide <- x_wide[levels(x_long[["col"]]), ]
    }
    if (is.factor(x_long[["col"]])) {
      x_wide <- x_wide[, levels(x_long[["col"]])]
    }
  }


  plt <- ggcorrhm(x_wide, annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df,
                  cell_labels = cell_label_df, cor_in = cor_in, ...)

  return(plt)
}


cor_long <- function(x, row1, col1, value1,
                     y = NULL, row2 = NULL, col2 = NULL, value2 = NULL,
                     out_format = c("wide", "long"),
                     method = "spearman", use = "everything") {
  x_long <- dplyr::select(x, {{row1}}, {{col1}}, {{value1}})
  # shape_mat_wide needs specific colnames
  colnames(x_long) <- c("row", "col", "value")
  x_long <- as.data.frame(x_long)
  x_wide <- shape_mat_wide(x_long)

  # Check if there should be a y matrix
  make_y <- !rlang::quo_is_null(rlang::enquo(row2)) &&
    !rlang::quo_is_null(rlang::enquo(col2)) &&
    !rlang::quo_is_null(rlang::enquo(value2))

  not_all_y <- any(
    !rlang::quo_is_null(rlang::enquo(row2)) ||
      !rlang::quo_is_null(rlang::enquo(col2)) ||
      !rlang::quo_is_null(rlang::enquo(value2))
  )

  if (not_all_y && !make_y) {
    cli::cli_abort("{.var {c('row2', 'col2', 'value2')}} all need to be not NULL to correlate two matrices.",
                   class = "too_few_args_error")
  }

  if (make_y) {
    # Either take columns for y from y input or use the x input
    y_long <- if (is.data.frame(y)) {
      dplyr::select(y, {{row2}}, {{col2}}, {{value2}})
    } else {
      dplyr::select(x, {{row2}}, {{col2}}, {{value2}})
    }
    colnames(y_long) <- c("row", "col", "value")
    y_long <- as.data.frame(y_long)
    y_wide <- shape_mat_wide(y_long)

    cor_out <- cor(x_wide, y_wide, method = method, use = use)
  } else {
    cor_out <- cor(x_wide, method = method, use = use)
  }

  # Convert to long format if desired
  if (out_format[1] == "long") {
    cor_out <- shape_mat_long(cor_out)
  }

  return(cor_out)
}
