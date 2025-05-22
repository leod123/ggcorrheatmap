#' Add dendrogram to ggplot object.
#'
#' @keywords internal
#'
#' @param ggp ggplot object to add dendrogram to.
#' @param dend_seg Dendrogram segment data obtained from the `prepare_dendrogram` function.
#' @param dend_col String specifying colour of dendrogram (used if the colours have not been changed using other options).
#' @param dend_lwd Line width of dendrogram segments (used if not changed using other options).
#' @param dend_lty Line type of dendrogram (used if not changed using other options).
#'
#' @returns A ggplot object with a dendrogram added.
#'
add_dendrogram <- function(ggp, dend_seg, dend_col = "black", dend_lwd = 0.3, dend_lty = 1) {
  if (all(is.na(dend_seg$col))) {
    # If the segment colours have not been changed, use the same colour for all segments
    ggp <- ggp +
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend), dend_seg,
                            colour = dend_col,
                            # Line width and type, same for all if not otherwise specified
                            linewidth = if (all(is.na(dend_seg$lwd))) {dend_lwd}
                            else {dend_seg$lwd},
                            linetype = if (all(is.na(dend_seg$lty))) {dend_lty}
                            else {dend_seg$lty})
  } else {
    seg_colr <- pull(distinct(dend_seg, col), col)
    names(seg_colr) <- seg_colr
    ggp <- ggp +
      ggnewscale::new_scale_colour() +
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, colour = col), dend_seg,
                            linewidth = if (all(is.na(dend_seg$lwd))) {dend_lwd}
                            else {dend_seg$lwd},
                            linetype = if (all(is.na(dend_seg$lty))) {dend_lty}
                            else {dend_seg$lty},
                            show.legend = F) +
      ggplot2::scale_colour_manual(values = seg_colr)
  }

  return(ggp)
}
