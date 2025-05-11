#' Mirror coordinates of dendrogram around its middle point
#'
#' @description
#' Mirror a dendrogram around an axis parallel to the height, located in its middle point
#'
#' @keywords internal
#'
#' @param dend_seg Data frame containing dendrogram segments, attained from `dendextend::as.ggdend()`
#' @param dend_dim Character specifying whether the dendrogram is linked to rows or columns in the heatmap
#'
#' @return Dendrogram segment coordinates dataframe, with the specified dimension mirrored
#'
mirror_dendrogram <- function(dend_seg, dend_dim = c("rows", "cols")) {

  stopifnot(dend_dim[1] %in% c("rows", "cols"))

  if (dend_dim[1] == "rows") {

    dend_middle <- (max(c(dend_seg$y, dend_seg$yend)) - min(c(dend_seg$y, dend_seg$yend))) / 2
    dend_seg$y <- dend_seg$y - 2 * (dend_seg$y - dend_middle)
    dend_seg$yend <- dend_seg$yend - 2 * (dend_seg$yend - dend_middle)

  } else {

    dend_middle <- (max(c(dend_seg$x, dend_seg$xend)) - min(c(dend_seg$x, dend_seg$xend))) / 2
    dend_seg$x <- dend_seg$x - 2 * (dend_seg$x - dend_middle)
    dend_seg$xend <- dend_seg$xend - 2 * (dend_seg$xend - dend_middle)

  }

  return(dend_seg)
}
