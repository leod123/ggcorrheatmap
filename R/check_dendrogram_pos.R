check_dendrogram_pos <- function(dat, dend_dim = c("row", "col"), dend) {
  coord_dim <- if (dend_dim[1] == "row") "y" else if (dend_dim[1] == "col") "x" else NA
  stopifnot(coord_dim %in% c("x", "y"))

  dend_lab <- dplyr::select(dend, lbl, coord_dim)
  dend_lab <- dplyr::filter(dend_lab, !is.na(lbl))
  # Take only distinct rows, in case a segment ends up on the same coordinate as the lowest node
  # Since they are originally float numbers, they may differ by a very small amount. Compare rounded versions
  dend_lab$coord_rounded <- round(dend_lab[[coord_dim]], 0)
  dend_lab <- dplyr::distinct(dend_lab, lbl, coord_rounded, .keep_all = T)

  # Add plot coordinates of labels
  dend_lab$plt_coord <- seq_along(levels(dat[[dend_dim[1]]]))[match(dend_lab$lbl, levels(dat[[dend_dim[1]]]))]

  # Check if same coordinates (within tolerance)
  stopifnot(all(dplyr::near(dend_lab[[coord_dim]], dend_lab[["plt_coord"]])))

  return(dend)
}
