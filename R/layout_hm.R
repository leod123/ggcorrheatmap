#' Layout heatmap data for plotting
#'
#' @param x Matrix to plot.
#' @param layout Layout (full, triangular (topleft, topright, bottomleft, bottomright)).
#' @param na_remove Logical indicating if NAs should be removed.
#'
#' @returns Long format data for plotting.
#'
layout_hm <- function(x, layout = "f", na_remove = F) {
  x <- as.matrix(x)

  if (isSymmetric(x) & layout %in% c("bottomleft", "bl", "topleft", "tl")) {
    x[upper.tri(x)] <- NaN

  } else if (isSymmetric(x) & layout %in% c("topright", "tr", "bottomright", "br")) {
    x[lower.tri(x)] <- NaN
  }

  # Get matrix in long format, remove NaN if triangular layout
  x_long <- shape_mat_long(x, na_remove = na_remove, nan_remove = T)

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
