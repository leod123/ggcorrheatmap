#' Calculate positions of annotations for heatmap
#'
#' @param annot_dim
#' @param annot_side
#' @param annot_names
#' @param data_size
#'
#' @return Numeric vector of annotation cell positions
#' @export
#'
#' @examples
get_annotation_coord <- function(annot_dim = c("rows", "cols"), annot_side = T, annot_names, data_size) {
  positions <- sapply(seq_along(annot_names), \(id) {
    ifelse(annot_side, 1, ncol(data)) +           # To middle of first or last column/row
      (0.5 + annot_rows_dist +                    # Add half of cell width + specified distance to heatmap
         0.5 * annot_rows_size +                  # Add half of annotation width (cell middle point is the coordinate for the cell)
         annot_rows_size * (id - 1) +             # Add width of previous annotations
         annot_rows_gap * (id - 1)) *             # Add gap between annotations
      ifelse(annot_left, -1, 1)                   # Subtract if left side, add if right side
  }, simplify = T) |> setNames(annot_names)

  return(positions)
}
