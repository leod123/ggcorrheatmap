#' Make a correlation matrix from long format data.
#'
#' @inheritParams ggcorrhm
#'
#' @param x A long format data frame containing the data to correlate.
#' @param rows,cols The columns in `x` containing the values that should be in the rows and columns of the correlation matrix.
#' @param values Name of the column in `x` containing the values of the correlation matrix.
#' @param y Optional second data frame for correlating with the data frame from `x`.
#' @param rows2,cols2 Optional names of columns with values for the rows and columns of a second matrix (taken from `y`).
#' @param values2 Optional column for the values of a second matrix.
#' @param out_format Format of output correlation matrix ("long" or "wide").
#' @param method Correlation method given to `stats::cor()`.
#' @param use Missing value strategy of `stats::cor()`.
#' @param p_values Logical indicating if p-values should be calculated.
#' @param p_sym_add String with the name of the column to add to p-value symbols from `p_thresholds` (one of 'values', 'p_val', 'p_adj').
#' NULL (default) results in just the symbols.
#' @param p_sym_digits Number of digits to use for the column in `p_sym_add`.
#'
#' @details
#' If there is only one input data frame (`x`), a wide matrix is constructed from `x` and passed to `stats::cor()`,
#' resulting in a correlation matrix with the column-column correlations.
#'
#' If `y` is a data frame and `rows2`, `cols2` and `values2` are specified, the wide versions of `x` and `y` are
#' correlated (`stats::cor(wide_x, wide_y)`) resulting in a correlation matrix with the columns of `x` in the
#' rows and the columns of `y` in the columns.
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
                     out_format = c("long", "wide"),
                     method = "pearson", use = "everything",
                     p_values = FALSE, p_adjust = "none",
                     p_thresholds = c("***" = 0.001, "**" = 0.01, "*" = 0.05, 1),
                     p_sym_add = NULL, p_sym_digits = 2) {

  if (missing(x)) cli::cli_abort("Argument {.var x} is missing. It needs to be a data frame.",
                                 class = "tidy_missing_error")
  if (missing(rows)) cli::cli_abort("Argument {.var rows} is missing. Provide the name of the column that contains the heatmap rownames.",
                                    class = "tidy_missing_error")
  if (missing(cols)) cli::cli_abort("Argument {.var cols} is missing. Provide the name of the column that contains the heatmap colnames.",
                                    class = "tidy_missing_error")
  if (missing(values)) cli::cli_abort("Argument {.var values} is missing. Provide the name of the column that contains the heatmap values.",
                                      class = "tidy_missing_error")

  # Check p-values and thresholds
  check_logical(p_values = p_values)
  if (isTRUE(p_values)) {
    check_numeric(p_sym_digits = p_sym_digits)

    if (!is.numeric(p_thresholds) && !is.null(p_thresholds)) {
      cli::cli_abort("{.var p_thresholds} must be a named {.cls numeric} vector or NULL.",
                     class = "p_thr_class_error")
    } else if (!is.null(p_thresholds)) {
      if (any(is.na(p_thresholds))) cli::cli_abort("{.var p_thresholds} should not contain any missing values.", class = "p_thr_error")
      if (any(p_thresholds <= 0)) cli::cli_abort("Values in {.var p_thresholds} must be above 0.", class = "p_thr_error")
      if (p_thresholds[length(p_thresholds)] < 1) cli::cli_abort("The last value of {.var p_thresholds} must be 1 or larger.", class = "p_thr_error")
      if (is.null(names(p_thresholds))) cli::cli_abort("{.var p_thresholds} must have named elements to be used as symbols (up to one unnamed).", class = "p_thr_error")
      if (any(duplicated(names(p_thresholds)))) cli::cli_abort("Symbols (the names) of {.var p_thresholds} must be unique.", class = "p_thr_error")
    }
  }

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
  make_y <- is.data.frame(y)

  not_all_y <- any(
    rlang::quo_is_null(rlang::enquo(rows2)) ||
      rlang::quo_is_null(rlang::enquo(cols2)) ||
      rlang::quo_is_null(rlang::enquo(values2))
  )

  if (not_all_y && make_y) {
    cli::cli_abort("{.var {c('y', 'rows2', 'cols2', 'values2')}} all need to be non-NULL to correlate two matrices.",
                   class = "tidy_too_few_args_error")
  }

  if (make_y) {
    # Either take columns for y from y input or use the x input
    y_long <- dplyr::select(y, {{rows2}}, {{cols2}}, {{values2}})

    if (ncol(y_long) > 3) {
      cli::cli_abort("Too many columns, provide one column each for {.var rows2}, {.var cols2} and {.var values2}.",
                     class = "tidy_too_many_cols_error")
    }

    colnames(y_long) <- c("row", "col", "value")
    y_long <- as.data.frame(y_long)
    y_wide <- shape_mat_wide(y_long)

    # Make arguments list for correlation function
    cor_args <- vector("list", 4)
    names(cor_args) <- c("x", "y", "method", "use")
    cor_args[["x"]] <- x_wide; cor_args[["y"]] <- y_wide

  } else {
    cor_args <- vector("list", 3)
    names(cor_args) <- c("x", "method", "use")
    cor_args[["x"]] <- x_wide
  }

  cor_args[["method"]] <- method; cor_args[["use"]] <- use

  if (isTRUE(p_values)) {
    cor_out <- do.call(test_cor, append(cor_args, list(p_adj_method = p_adjust)))

    if (!is.null(p_thresholds)) {
      # Add asterisks
      cor_out[["p_sym"]] <- as.character(symnum(x = cor_out[["p_adj"]],
                                                cutpoints = c(0, p_thresholds),
                                                symbols = names(p_thresholds)))

      if (!is.null(p_sym_add) && p_sym_add %in% c("value", "p_val", "p_adj")) {
        cor_out[["p_sym"]] <- paste0(round(cor_out[[p_sym_add]], p_sym_digits),
                                     cor_out[["p_sym"]])
      } else if (!is.null(p_sym_add)) {
        cli::cli_warn("{.var p_sym_add} should be one of {.val {c('value', 'p_val', 'p_adj')}} to
                      specify which column to add to the p-value symbols, or NULL for just the symbols.",
                      class = "p_sym_option_warn")
      }

    } else {
      cor_out[["p_sym"]] <- NA
    }

    # test_cor gives the results as a long data frame, convert to wide if desired
    # (one each for correlation, p-values, adj p, asterisks)
    if (out_format[1] == "wide") {
      cor_out <- sapply(c("value", "p_val", "p_adj", "p_sym"), function(column) {
        shape_mat_wide(dplyr::select(cor_out, row, col, value = !!column))
      }, simplify = FALSE, USE.NAMES = TRUE)
    }

  } else {
    cor_out <- do.call(stats::cor, cor_args)
    # Convert to long format if desired
    if (out_format[1] == "long") {
      cor_out <- shape_mat_long(cor_out)
    }
  }

  return(cor_out)
}
