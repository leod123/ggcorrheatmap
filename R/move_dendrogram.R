#' Move coordinates of dendrogram to edges of heatmap
#'
#' @description
#' Move coordinates of dendrogram by calculating distance it has to move to end up at the desired edges of the heatmap
#'
#' @keywords internal
#'
#' @param cor_long Long format data frame with the correlations
#' @param dend_seg Data frame containing dendrogram segments, attained from `dendextend::as.ggdend()`
#' @param dend_dim Character specifying whether the dendrogram is linked to rows or columns in the heatmap
#' @param dend_side Logical specifying dendrogram position. TRUE is left of the heatmap if row dendrogram, bottom of heatmap if column dendrogram
#' @param annot_df Data frame with annotations for checking that annotations exist as well as their size
#' @param annot Logical indicating if annotations are intended to be drawn or not
#' @param annot_side Logical specifying annotation position, analogous to `dend_side`
#' @param annot_pos Numeric vector of annotation coordinates (x coordinates for row annotations, y for column annotations)
#' @param annot_size Numeric of length 1, the specified size (width or height) of annotation cells
#'
#' @return Data frame with updated dendrogram coordinates
#'
move_dendrogram <- function(cor_long, dend_seg, dend_dim = c("rows", "cols"), dend_side,
                            annot_df, annot, annot_side, annot_pos, annot_size) {

  # Check that dendrogram dimension (rows or columns) is specified properly
  stopifnot(dend_dim[1] %in% c("rows", "cols"))

  if (dend_dim[1] == "rows") {

    xmove <- ifelse(
      # Dendrogram on left or right side
      dend_side,
      ifelse(    # Annotation left of rows or not
        is.data.frame(annot_df) & annot & annot_side,
        min(annot_pos) - 0.5 * annot_size - max(c(dend_seg$x, dend_seg$xend)),
        0.5 - max(c(dend_seg$x, dend_seg$xend))
      ),
      ifelse(    # Annotation right of rows or not
        is.data.frame(annot_df) & annot & !annot_side,
        max(annot_pos) + 0.5 * annot_size - min(c(dend_seg$x, dend_seg$xend)),
        length(unique(cor_long$col)) + 0.5 - min(c(dend_seg$x, dend_seg$xend))
      )
    )

    ymove <- length(unique(cor_long$row)) - max(c(dend_seg$y, dend_seg$yend))

  } else {

    xmove <- 1 - min(c(dend_seg$x, dend_seg$xend))

    ymove <- ifelse(
      # Dendrogram above or below heatmap
      dend_side,
      ifelse(    # Annotation below heatmap
        is.data.frame(annot_df) & annot & annot_side,
        min(annot_pos) - 0.5 * annot_size - max(c(dend_seg$y, dend_seg$yend)),
        0.5 - max(c(dend_seg$y, dend_seg$yend))
      ),
      ifelse(    # Annotation above heatmap
        is.data.frame(annot_df) & annot & !annot_side,
        max(annot_pos) + 0.5 * annot_size - min(c(dend_seg$y, dend_seg$yend)),
        length(unique(cor_long$col)) + 0.5 - min(c(dend_seg$y, dend_seg$yend))
      )
    )
  }

  dend_seg[, c("x", "xend")] <- dend_seg[, c("x", "xend")] + xmove
  dend_seg[, c("y", "yend")] <- dend_seg[, c("y", "yend")] + ymove

  return(dend_seg)
}
