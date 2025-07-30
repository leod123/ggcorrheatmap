#' Convert a matrix to long format using row names and column names.
#'
#' @keywords internal
#'
#' @param x Matrix to convert to long format.
#' @param unique_pairs Whether only unique combinations should be included in the output (for symmetric matrices).
#' @param na_remove Logical indicating if NAs should be excluded (removes NaNs too).
#'
#' @returns A data frame with the columns 'row', 'column' (indicating combinations), and 'value'
#'
shape_mat_long <- function(x, unique_pairs = FALSE, na_remove = FALSE) {
  rowcol <- value <- NULL

  check_logical(na_remove = na_remove)

  # Convert to long format
  mat_long <- as.data.frame(x)
  mat_long$.row <- rownames(mat_long)

  cols <- setdiff(colnames(mat_long), ".row")
  mat_long <- reshape(mat_long, direction = "long", timevar = ".col", v.names = "value",
                      varying = cols, times = cols)

  # Remove repeated combinations if desired
  if (unique_pairs) {
    # Make a new column with sorted row-col combinations and filter out unique rows
    mat_long$rowcol <- apply(mat_long, 1, function(i) paste(sort(c(i[".row"], i[".col"])), collapse = "_"))
    mat_long <- dplyr::distinct(mat_long, rowcol, .keep_all = TRUE)
  }

  # Remove NAs if desired (makes for completely empty cells and a cleaner look) (also removes NaN)
  if (isTRUE(na_remove)) {
    mat_long <- dplyr::filter(mat_long, !is.na(value))
  }

  # Keep only the necessary columns and remove rownames to prevent problems with downstream functions
  mat_long <- dplyr::select(mat_long, row = .row, col = .col, value)
  rownames(mat_long) <- NULL

  return(mat_long)
}


#' Convert long format matrix to wide.
#'
#' @keywords internal
#'
#' @param x Long format matrix as obtained from 'shape_mat_long' (contains columns named 'row', 'col', and 'value').
#'
#' @returns Wide format version of x.
#'
shape_mat_wide <- function(x) {
  mat_wide <- reshape(x, idvar = "row", timevar = "col", direction = "wide")
  rownames(mat_wide) <- mat_wide[["row"]]
  mat_wide <- mat_wide[, -which(colnames(mat_wide) == "row")]
  # Remove "value." from colnames
  mat_wide <- dplyr::rename_with(mat_wide, function(nm) {substring(nm, 7)})
  mat_wide <- as.matrix(mat_wide)

  return(mat_wide)
}
