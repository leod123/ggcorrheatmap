#' Convert a matrix to long format using row names and column names.
#'
#' @param x Matrix to convert to long format.
#' @param unique_pairs Whether only unique combinations should be included in the output (for symmetric matrices).
#'
#' @returns A data frame with the columns 'row', 'column' (indicating combinations), and 'value'
#' @export
#'
#' @examples
#' shape_mat_long(cor(mtcars))
shape_mat_long <- function(x, unique_pairs = F) {
  # Remove upper triangle of correlation matrix if only unique pairs are desired
  if (unique_pairs) {
    x[upper.tri(x, diag = F)] <- NA
  }

  # Convert to long format
  mat_long <- as.data.frame(x)
  mat_long$.row <- rownames(mat_long)

  cols <- setdiff(colnames(mat_long), ".row")
  mat_long <- reshape(mat_long, direction = "long", timevar = ".col", v.names = "value",
                      varying = cols, times = cols)
  # Remove NAs, unneeded columns, and rownames
  mat_long <- dplyr::filter(mat_long, !is.na(value))
  mat_long <- dplyr::select(mat_long, row = .row, col = .col, value)
  rownames(mat_long) <- NULL

  return(mat_long)
}
