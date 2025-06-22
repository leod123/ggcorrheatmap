#' Convert a matrix to long format using row names and column names.
#'
#' @keywords internal
#'
#' @param x Matrix to convert to long format.
#' @param unique_pairs Whether only unique combinations should be included in the output (for symmetric matrices).
#' @param na_remove Logical indicating if NAs should be excluded (removes NaNs too).
#' @param nan_remove Logical indicating if NaNs should be excluded (if want to keep NAs but remove some cells that are NaN).
#'
#' @returns A data frame with the columns 'row', 'column' (indicating combinations), and 'value'
#'
#' @examples
#' \dontrun{
#' shape_mat_long(cor(mtcars))
#' }
shape_mat_long <- function(x, unique_pairs = F, na_remove = F) {
  rowcol <- value <- NULL

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
    mat_long <- dplyr::distinct(mat_long, rowcol, .keep_all = T)
  }

  # Remove NAs if desired (makes for completely empty cells and a cleaner look) (also removes NaN)
  if (na_remove) {
    mat_long <- dplyr::filter(mat_long, !is.na(value))
  }

  # Keep only the necessary columns and remove rownames to prevent problems with downstream functions
  mat_long <- dplyr::select(mat_long, row = .row, col = .col, value)
  rownames(mat_long) <- NULL

  return(mat_long)
}
