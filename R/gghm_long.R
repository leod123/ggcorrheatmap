#' gghm for long format data
#'
#' @param x
#' @param rows Name of column to use as rows.
#' @param cols Name of column to use as columns.
#' @param values Name of column to use for cell values.
#' @param annot_rows
#' @param annot_cols
#' @param ...
gghm_long <- function(x, rows, cols, values, labels = NULL, annot_rows = NULL, annot_cols = NULL, ...) {

  if (missing(x)) cli::cli_abort("Argument {.var x} is missing!")
  if (missing(rows)) cli::cli_abort("Argument {.var rows} is missing!")
  if (missing(cols)) cli::cli_abort("Argument {.var cols} is missing!")

  # Move this to a separate function and reuse in ggcorrhm_long
  x_long <- dplyr::select(x, {{rows}}, {{cols}}, {{values}})

  # Check that rows and cols are not too many
  if (ncol(x_long) > 3) {
    cli::cli_abort("Too many columns, provide one column each for {.var rows}, {.var cols} and {.var values}.")
  }

  # Convert to wide format
  colnames(x_long)[1:3] <- c("row", "col", "value")
  x_long <- as.data.frame(x_long)
  x_wide <- shape_mat_wide(x_long)

  # Reorder rows and columns if input data has factor levels

  # Cell labels
  if (!rlang::quo_is_null(rlang::enquo(labels))) {
    cell_label_df <- dplyr::select(x, {{rows}}, {{cols}}, {{labels}})
    colnames(cell_label_df) <- c("row", "col", "value")
    cell_label_df <- as.data.frame(cell_label_df)
    cell_label_df <- shape_mat_wide(cell_label_df)

  } else {
    cell_label_df <- NULL
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

ggcorrhm_long <- function(x, rows, cols, values, annot_rows = NULL, annot_cols = NULL,

                          ...) {
  # rows and cols are the names of columns that will be in the matrix that will be correlated?
  # And then the output has only the colnames
  # how should it work for two inputs? all cols in the same df or two dfs?
  # allow only one input matrix for the long version?
  # annotation only uses cols unlike gghm_long?
  # what about cell labels?

  if (missing(x)) cli::cli_abort("Argument {.var x} is missing!")
  if (missing(rows)) cli::cli_abort("Argument {.var rows} is missing!")
  if (missing(cols)) cli::cli_abort("Argument {.var cols} is missing!")

  # Move this to a separate function and reuse in ggcorrhm_long
  x_long <- dplyr::select(x, {{rows}}, {{cols}}, {{values}})

  # Check that rows and cols are not too many
  if (ncol(x_long) > 3) {
    cli::cli_abort("Too many columns, provide one column each for {.var rows}, {.var cols} and {.var values}.")
  }

  # Convert to wide format
  colnames(x_long)[1:3] <- c("row", "col", "value")
  x_long <- as.data.frame(x_long)
  x_wide <- shape_mat_wide(x_long)

  # Reorder rows and columns if input data has factor levels


  # Annotation
  ### How to do when two input matrices???
  # Make the annotation data frames if any columns have been provided
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

  plt <- ggcorrhm(x_wide, annot_rows_df = annot_rows_df, annot_cols_df = annot_cols_df, ...)

  return(plt)
}
