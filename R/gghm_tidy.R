#' `gghm()` for long format data.
#'
#' @param x Data frame containing data to plot.
#' @param rows,cols,values Columns to use as rows, columns, and cell values.
#' @param labels Column to use for cell labels. NULL (default) for no labels.
#' @param annot_rows,annot_cols Columns to use for row and column annotations.
#' @param facet_rows,facet_cols Columns to use for row/column facet memberships.
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
gghm_tidy <- function(x, rows, cols, values, labels = NULL, annot_rows = NULL, annot_cols = NULL,
                      facet_rows = NULL, facet_cols = NULL, ...) {

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

  # The input parameters for possibly overwriting some
  gghm_in <- list(...)

  # If col_name and size_name are not specified, use the name of the variable column
  if (!"col_name" %in% names(gghm_in)) {
    gghm_in[["col_name"]] <- colnames(x_long)[3]
  }
  if (!"size_name" %in% names(gghm_in)) {
    gghm_in[["size_name"]] <- colnames(x_long)[3]
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

  # Make facet vectors
  row_facet_vec <- prepare_facets_tidy(x, {{rows}}, {{facet_rows}}, gghm_in, "row")
  col_facet_vec <- prepare_facets_tidy(x, {{cols}}, {{facet_cols}}, gghm_in, "column")

  gghm_in[["split_rows"]] <- row_facet_vec
  gghm_in[["split_cols"]] <- col_facet_vec

  plt <- do.call(gghm, append(
    gghm_in,
    list(x = x_wide, cell_labels = cell_label_df,
         annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df)
  ))

  return(plt)
}

#' `ggcorrhm()` for long format data.
#'
#' @param x Data containing data to plot or to correlate.
#' @param rows,cols,values Columns to use as rows, columns, and values in the plotted matrix (if `cor_in` is TRUE) or the matrix to compute correlations from (`cor_in` is FALSE).
#' @param annot_rows,annot_cols Columns containing values for row and column annotations.
#' @param labels Column to use for cell labels, NULL for no labels, or TRUE to use the cell values. If `cor_in` is FALSE, only NULL, TRUE or FALSE is supported.
#' @param facet_rows,facet_cols Columns to use for row/column facets.
#' @param cor_in Logical indicating if the values are correlation values (TRUE, default) or values to be correlated. See details for more information.
#' @param ... Additional arguments for `ggcorrhm()`.
#'
#' @details
#' If `cor_in` is TRUE (the default), `ggcorrhm_tidy()` behaves similarly to `gghm_tidy()` but with the colour
#' scales and arguments of `ggcorrhm()` instead of `gghm()`.
#'
#' If `cor_in` FALSE, the data is converted to wide format and the column-column correlations are computed.
#' This means that if non-square correlation matrices are to be plotted the correlations have to be computed
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
                          labels = NULL, facet_rows = NULL, facet_cols = NULL, cor_in = TRUE, ...) {

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

  # The input parameters for possibly overwriting some
  ggcorrhm_in <- list(...)

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

    # Fix facet input
    row_facet_vec <- prepare_facets_tidy(x, {{rows}}, {{facet_rows}}, ggcorrhm_in, "rows")
    col_facet_vec <- prepare_facets_tidy(x, {{cols}}, {{facet_cols}}, ggcorrhm_in, "columns")

    ggcorrhm_in[["split_rows"]] <- row_facet_vec
    ggcorrhm_in[["split_cols"]] <- col_facet_vec

    # Reorder rows and columns if input data has factor levels
    if (is.factor(x_long[["row"]])) {
      x_wide <- x_wide[levels(x_long[["row"]]), ]
    }
    if (is.factor(x_long[["col"]])) {
      x_wide <- x_wide[, levels(x_long[["col"]])]
    }

  } else {
    # Work like in ggcorrhm if cor_in is FALSE
    if (is.null(labels)) {
      cell_label_df <- FALSE
    } else {
      cell_label_df <- labels
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

    row_facet_vec <- prepare_facets_tidy(x, {{cols}}, {{facet_rows}}, ggcorrhm_in, "rows")
    col_facet_vec <- prepare_facets_tidy(x, {{cols}}, {{facet_cols}}, ggcorrhm_in, "columns")

    ggcorrhm_in[["split_rows"]] <- row_facet_vec
    ggcorrhm_in[["split_cols"]] <- col_facet_vec

    if (is.factor(x_long[["row"]])) {
      x_wide <- x_wide[levels(x_long[["col"]]), ]
    }
    if (is.factor(x_long[["col"]])) {
      x_wide <- x_wide[, levels(x_long[["col"]])]
    }
  }

  plt <- do.call(ggcorrhm, append(
    ggcorrhm_in,
    list(x = x_wide, annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df,
         cell_labels = cell_label_df, cor_in = cor_in)
  ))

  return(plt)
}

