#' `gghm()` for long format data.
#'
#' @param x Data frame containing data to plot.
#' @param rows,cols,values Columns to use as rows, columns, and cell values.
#' @param labels Column to use for cell labels. NULL (default) for no labels.
#' @param annot_rows Columns to use for row annotations.
#' @param annot_cols Columns to use for column annotations.
#' @param ... Additional arguments for `gghm()`.
#'
#' @return A ggplot2 object with the heatmap. If `return_data` is `TRUE`, plotting data is returned as well.
#' @export
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
#' # Columns are given using 'tidy' selection
#' # so they can be unquoted, quoted, from variables (with !! notation) or indices
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

  if (missing(x)) cli::cli_abort("Argument {.var x} is missing. It needs to be a data frame.",
                                 class = "tidy_missing_error")
  if (missing(rows)) cli::cli_abort("Argument {.var rows} is missing. Provide the name of the column that contains the heatmap rownames.",
                                    class = "tidy_missing_error")
  if (missing(cols)) cli::cli_abort("Argument {.var cols} is missing. Provide the name of the column that contains the heatmap colnames.",
                                    class = "tidy_missing_error")
  if (missing(values)) cli::cli_abort("Argument {.var values} is missing. Provide the name of the column that contains the heatmap values.",
                                      class = "tidy_missing_error")

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

    # If values and labels use the same column, set cell_label_df to TRUE instead
    # so that it can be affected by scaling
    if (identical(rlang::as_name(rlang::enquo(values)), rlang::as_name(rlang::enquo(labels)))) {
      cell_label_df <- TRUE
    } else {
      colnames(cell_label_df) <- c("row", "col", "value")
      cell_label_df <- as.data.frame(cell_label_df)
      cell_label_df <- shape_mat_wide(cell_label_df)
    }

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

#' `ggcorrhm()` for long format data.
#'
#' @param x Data containing data to plot or to correlate.
#' @param rows,cols,values Columns to use as rows, columns, and values in the plotted matrix (if `cor_in` is TRUE) or the matrix to compute correlations from (`cor_in` is FALSE).
#' @param annot_rows,annot_cols Columns containing values for row and column annotations.
#' @param labels Column to use for cell labels, NULL for no labels, or TRUE to use the cell values. If `cor_in` is FALSE, only NULL, TRUE or FALSE is supported.
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
#' On the other hand, if `cor_in` is TRUE any computation of correlations is skipped, meaning that p-values
#' cannot be computed and would have to be generated in advance and passed as cell labels.
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

  if (missing(x)) cli::cli_abort("Argument {.var x} is missing. It needs to be a data frame.",
                                 class = "tidy_missing_error")
  if (missing(rows)) cli::cli_abort("Argument {.var rows} is missing. Provide the name of the column that contains the heatmap rownames.",
                                    class = "tidy_missing_error")
  if (missing(cols)) cli::cli_abort("Argument {.var cols} is missing. Provide the name of the column that contains the heatmap colnames.",
                                    class = "tidy_missing_error")
  if (missing(values)) cli::cli_abort("Argument {.var values} is missing. Provide the name of the column that contains the heatmap values.",
                                      class = "tidy_missing_error")

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


#' Make a correlation matrix from long format data.
#'
#' @param x A long format data frame containing the data to correlate.
#' @param rows,cols The columns in `x` containing the values that should be in the rows and columns of the correlation matrix.
#' @param values Name of the column in `x` containing the values of the correlation matrix.
#' @param y Optional second data frame for correlating with the data frame from `x`.
#' @param rows2,cols2 Optional names of columns with values for the rows and columns of a second matrix. If `y` is a data frame, `rows2` is taken from `y`.
#' Otherwise it is taken from `x`.
#' @param values2 Optional column for the values of a second matrix.
#' @param out_format Format of output correlation matrix ("long" or "wide").
#' @param method Correlation method given to `stats::cor()`.
#' @param use Missing value strategy of `stats::cor()`.
#'
#' @details
#' If there is only one input data frame (`x`) and `rows2`, `cols2` and `values2` are NULL (the default),
#' a wide matrix is constructed from `x` and passed to `stats::cor()`, resulting in a correlation matrix
#' with the column-column correlations.
#'
#' If `y` is a data frame and `rows2`, `cols2` and `values2` are specified, the wide versions of `x` and `y` are
#' correlated (`stats::cor(wide_x, wide_y)`) resulting in a correlation matrix with the columns of `x` in the
#' rows and the columns of `y` in the columns.
#'
#' If `y` is NULL but `rows2`, `cols2` and `values2` are specified, the columns are taken from `x`,
#' creating two matrices to correlate from `x`.
#'
#' @returns A correlation matrix (if wide format) or a long format data frame with the columns
#' 'row', 'col', and 'value' (containing correlations).
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' cor_in <- data.frame(row = rep(letters[1:10], each = 5),
#'                      col = rep(LETTERS[1:5], 10),
#'                      val = rnorm(50))
#' # Wide format output (default)
#' corr_wide <- cor_long(cor_in, row, col, val)
#'
#' # Long format output
#' corr_long <- cor_long(cor_in, row, col, val,
#'                       out_format = "long")
#'
#' # Correlation between two matrices
#' cor_in2 <- data.frame(rows = rep(letters[1:10], each = 10),
#'                       cols = rep(letters[1:10], 10),
#'                       values = rnorm(100))
#' corr2 <- cor_long(cor_in, row, col, val,
#'                   cor_in2, rows, cols, values)
#'
cor_long <- function(x, rows, cols, values,
                     y = NULL, rows2 = NULL, cols2 = NULL, values2 = NULL,
                     out_format = c("wide", "long"),
                     method = "pearson", use = "everything") {

  if (missing(x)) cli::cli_abort("Argument {.var x} is missing. It needs to be a data frame.")
  if (missing(rows)) cli::cli_abort("Argument {.var rows} is missing. Provide the name of the column that contains the heatmap rownames.")
  if (missing(cols)) cli::cli_abort("Argument {.var cols} is missing. Provide the name of the column that contains the heatmap colnames.")
  if (missing(values)) cli::cli_abort("Argument {.var values} is missing. Provide the name of the column that contains the heatmap values.")

  x_long <- dplyr::select(x, {{rows}}, {{cols}}, {{values}})

  if (ncol(x_long) > 3) {
    cli::cli_abort("Too many columns, provide one column each for {.var rows}, {.var cols} and {.var values}.",
                   class = "tidy_too_many_cols_error")
  }

  # shape_mat_wide needs specific colnames
  colnames(x_long) <- c("row", "col", "value")
  x_long <- as.data.frame(x_long)
  x_wide <- shape_mat_wide(x_long)

  # Check if there should be a y matrix
  make_y <- !rlang::quo_is_null(rlang::enquo(rows2)) &&
    !rlang::quo_is_null(rlang::enquo(cols2)) &&
    !rlang::quo_is_null(rlang::enquo(values2))

  not_all_y <- any(
    !rlang::quo_is_null(rlang::enquo(rows2)) ||
      !rlang::quo_is_null(rlang::enquo(cols2)) ||
      !rlang::quo_is_null(rlang::enquo(values2))
  )

  if (not_all_y && !make_y) {
    cli::cli_abort("{.var {c('rows2', 'cols2', 'values2')}} all need to be not NULL to correlate two matrices.",
                   class = "too_few_args_error")
  }

  if (make_y) {
    # Either take columns for y from y input or use the x input
    y_long <- if (is.data.frame(y)) {
      dplyr::select(y, {{rows2}}, {{cols2}}, {{values2}})
    } else {
      dplyr::select(x, {{rows2}}, {{cols2}}, {{values2}})
    }
    if (ncol(y_long) > 3) {
      cli::cli_abort("Too many columns, provide one column each for {.var rows2}, {.var cols2} and {.var values2}.",
                     class = "tidy_too_many_cols_error")
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
