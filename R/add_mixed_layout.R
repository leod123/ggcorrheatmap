#' Add a layout column to long format data for mixed layouts.
#'
#' @param x Long format data frame of a symmetric matrix.
#' @param rows,cols,values Columns containing rows, columns, and values.
#' @param layout Character vector of length two with a mixed layout (two opposing triangles).
#' @param name Name of the column that should contain the layouts.
#'
#' @returns The input data frame with a new column added, showing in which triangle each value would be in a mixed layout.
#' @export
#'
#' @examples
#' # Make long format symmetric data
#' long_df <- data.frame(rw = rep(letters[1:4], 4),
#'                       cl = rep(letters[1:4], each = 4),
#'                       val = 0)
#'
#' long_df <- add_mixed_layout(long_df, rw, cl, val,
#'                             layout = c("topleft", "bottomright"))
#'
#' head(long_df)
#'
add_mixed_layout <- function(x, rows = "row", cols = "col", values = "value",
                             layout, name = "layout") {

  # Make a wide version of the input matrix
  x_wide <- dplyr::select(x, {{rows}}, {{cols}}, {{values}})

  if (ncol(x_wide) > 3) {
    cli::cli_abort("Too many columns, provide one column each for {.var rows}, {.var cols} and {.var values}.",
                   class = "tidy_too_many_cols_error")
  }

  # Keep the old colnames for later
  colnm <- colnames(x_wide)

  # Change to the defaults used in shape_mat_wide
  colnames(x_wide) <- c("row", "col", "value")
  # Convert a tibble to a data frame to support row names
  if (inherits(x_wide, "tbl_df")) {x_wide <- as.data.frame(x_wide)}

  x_wide <- shape_mat_wide(x_wide)

  # Check that it's symmetric
  if (!isSymmetric(as.matrix(x_wide))) {
    cli::cli_abort("The wide version of {.var x} must be symmetric.",
                   class = "nonsym_error")
  }

  # Check that the layout is ok (must be mixed)
  if (length(layout) == 1) {
    cli::cli_abort("Layout must be a length two vector in mixed modes.",
                   class = "nonsup_layout_error")
  }
  check_layout(layout, mode = c("hm", "hm"))

  # Check that the output column name does not already exist
  if (name %in% colnames(x)) {
    cli::cli_warn("{.val {name}} already exists in the input, writing layout to
                   {.val {paste0('.', name)}} instead.",
                  class = "name_exists_warn")
    name <- paste0(".", name)
  }

  # Get row and column values in the first triangle and add layout
  tri1 <- layout_hm(x_wide, layout[1])
  tri1[[name]] <- layout[1]

  # Second triangle
  tri2 <- layout_hm(x_wide, layout[2])
  tri2 <- dplyr::filter(tri2, as.character(row) != as.character(col))
  tri2[[name]] <- layout[2]

  # Add the layouts to the input data
  tri12 <- dplyr::bind_rows(tri1, tri2)
  # Make the `by` argument vector, use the old colnames
  by_vec <- c("row", "col", "value")
  names(by_vec) <- colnm
  x <- dplyr::left_join(x, tri12, by = by_vec)

  return(x)
}
