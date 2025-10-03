#' Layout heatmap data for plotting
#'
#' @keywords internal
#'
#' @param x Matrix to plot.
#' @param layout Layout (full, triangular (topleft, topright, bottomleft, bottomright)).
#' @param na_remove Logical indicating if NAs should be removed.
#'
#' @returns Long format data for plotting.
#'
layout_hm <- function(x, layout = "f", na_remove = FALSE) {

  if (isSquare(as.matrix(x)) & layout %in% c("bottomleft", "bl", "topleft", "tl")) {
    x_long <- remove_triangle(x, tri_remove = "upper", na_remove = na_remove)

  } else if (isSquare(as.matrix(x)) & layout %in% c("topright", "tr", "bottomright", "br")) {
    x_long <- remove_triangle(x, tri_remove = "lower", na_remove = na_remove)

  } else {
    x_long <- shape_mat_long(x, na_remove = na_remove)

  }

  # Reverse order of y-axis (rows) for specific layouts
  x_long$row <- factor(x_long$row, levels =
                         if (layout %in% c("topleft", "tl", "bottomright", "br")) {
                           rownames(x)
                         } else {
                           rev(rownames(x))
                         })
  x_long$col <- factor(x_long$col, levels = colnames(x))

  return(x_long)
}

#' Remove triangle from symmetric matrix and return long format data.
#'
#' @keywords internal
#'
#' @param x Matrix to remove triangle from (and make long).
#' @param tri_remove Triangle to remove.
#' @param na_remove If NAs should be removed.
#'
#' @returns Matrix in long format with triangle removed.
#'
remove_triangle <- function(x, tri_remove = "upper", na_remove = FALSE) {
  # Make the base for the output
  x_out <- shape_mat_long(x, na_remove = na_remove)

  # Make a matrix to get the rows that should be removed
  x_remove <- as.matrix(x)
  # Fill with ones (to remove any potential NAs or NaNs), add NAs
  x_remove[] <- 1
  # Keep rows to remove in the original input
  if (tri_remove == "upper") {
    x_remove[lower.tri(x_remove, diag = TRUE)] <- NA
  } else if (tri_remove == "lower") {
    x_remove[upper.tri(x_remove, diag = TRUE)] <- NA
  }
  x_remove <- shape_mat_long(x_remove, na_remove = TRUE)
  # Get rows to remove, using a very uncommon separator to not accidentally remove too many
  rows_remove <- paste0(x_remove$row, "_;%?!_", x_remove$col)

  x_out <- x_out[which(!paste0(x_out$row, "_;%?!_", x_out$col) %in% rows_remove), ]
  rownames(x_out) <- NULL
  return(x_out)
}
