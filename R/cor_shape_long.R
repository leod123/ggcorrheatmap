#' Convert a correlation matrix (symmetric) to long format
#'
#' @param x Correlation matrix to convert (output of `stats::cor`).
#' @param unique_pairs Whether only unique combinations should be included in the output.
#'
#' @returns A data frame with the columns 'row', 'column' (indicating combinations), and 'cor' (correlation coefficients).
#' @export
#'
#' @examples
#' cor_shape_long(cor(mtcars))
cor_shape_long <- function(x, unique_pairs = F) {
  # Remove upper triangle of correlation matrix if only unique pairs are desired
  if (unique_pairs) {
    x[upper.tri(x, diag = F)] <- NA
  }

  # Convert to long format
  cor_mat_long <- as.data.frame(x)
  cor_mat_long$.row <- rownames(cor_mat_long)

  cols <- setdiff(colnames(cor_mat_long), ".row")
  cor_mat_long <- reshape(cor_mat_long, direction = "long", timevar = ".col", v.names = "cor",
                          varying = cols, times = cols)
  # Remove NAs, unneeded columns, and rownames
  cor_mat_long <- dplyr::filter(cor_mat_long, !is.na(cor))
  cor_mat_long <- dplyr::select(cor_mat_long, row = .row, col = .col, cor)
  rownames(cor_mat_long) <- NULL

  return(cor_mat_long)
}
