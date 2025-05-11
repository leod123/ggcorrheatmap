#' Scale height of dendrogram.
#'
#' @keywords internal
#'
#' @param dend_seg Data frame containing dendrogram segments in format obtained from `dendextend`.
#' @param dend_dim Dimension to draw dendrogram along (rows or columns).
#' @param dend_side Logical indicating which side to draw dendrogram on.
#' If row dendrogram TRUE is left. If column dendrogram TRUE is down.
#' @param dend_height Scaling parameter for dendrogram height (1 is no scaling).
#'
#' @return Data frame containing coordinates for dendrogram segments (and any colour, linewidth, line type parameters)
#'
scale_dendrogram <- function(dend_seg, dend_dim = c("rows", "cols"), dend_side, dend_height) {

  stopifnot(dend_dim[1] %in% c("rows", "cols"))

  if (dend_dim[1] == "rows") {
    dend_seg_out <- dplyr::mutate(
      dend_seg,
      # Move horizontal segments by scaling the distance from the points touching the heatmap
      x = ifelse(dend_side, max(xend), min(xend)) +
        (x - ifelse(dend_side, max(xend), min(xend))) * dend_height,
      xend = ifelse(dend_side, max(xend), min(xend)) +
        (xend - ifelse(dend_side, max(xend), min(xend))) * dend_height
    )
    # dend_seg_out[["x"]] <- ifelse(dend_side, max(dend_seg_out[["xend"]]), min(dend_seg_out[["xend"]])) +
    #   (dend_seg_out[["x"]] - ifelse(dend_side, max(dend_seg_out[["xend"]]), min(dend_seg_out[["xend"]]))) * dend_height
    # dend_seg_out[["xend"]] <- ifelse(dend_side, max(dend_seg_out[["xend"]]), min(dend_seg_out[["xend"]])) +
    #   (dend_seg_out[["xend"]] - ifelse(dend_side, max(dend_seg_out[["xend"]]), min(dend_seg_out[["xend"]]))) * dend_height
  } else {
    dend_seg_out <- dplyr::mutate(
      dend_seg,
      y = ifelse(!dend_side, min(yend), max(yend)) +
        (y - ifelse(!dend_side, min(yend), max(yend))) * dend_height,
      yend = ifelse(!dend_side, min(yend), max(yend)) +
        (yend - ifelse(!dend_side, min(yend), max(yend))) * dend_height
    )
  }

  return(dend_seg_out)
}
