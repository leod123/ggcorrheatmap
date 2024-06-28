#' Add annotations to ggplot heatmap
#'
#' @param ggp `ggplot` object with `geom_tile` layer to add annotations to
#' @param annot_dim Dimension to add annotations to, either "rows" or "columns"
#' @param annot_df Data frame containing the annotations.
#' The first column must contain the labels used in the heatmap rows or columns.
#' Each of the other columns should contain corresponding annotations. The column names are used as labels in the legend.
#' @param annot_pos Positions of annotations. x positions if row annotations, y positions if column annotations.
#' Positions are given by the middle coordinate of the annotation cells.
#' @param annot_size Size of annotations, width of row annotations, height if column annotations.
#' @param annot_border_lwd Linewidth of border lines of annotation cells.
#' @param annot_border_col Colour of border lines of annotation cells.
#'
#' @return `ggplot` object with added annotations.
#' @export
#'
#' @examples
add_annotation <- function(ggp, annot_dim = c("rows", "cols"), annot_df, annot_pos, annot_size,
                           annot_border_lwd = 0.5, annot_border_col = "grey") {

  annot_names <- colnames(annot_df)[-1]

  plt_out <- ggp +
    lapply(annot_names, \(nm) {
      list(
        # Add separate colour scale for each annotation
        ggnewscale::new_scale_fill(),

        # Draw annotation
        ggplot2::geom_tile(data = annot_df,
                           mapping = aes(x = if (annot_dim[1] == "rows") {annot_pos[nm]}
                                         else {.data[[colnames(annot_df)[1]]]},
                                         y = if (annot_dim[1] == "rows") {.data[[colnames(annot_df)[1]]]}
                                         else {annot_pos[nm]},
                                         fill = .data[[nm]]),
                           width = ifelse(annot_dim[1] == "rows", annot_size, 1),
                           height = ifelse(annot_dim[1] == "rows", 1, annot_size),
                           linewidth = annot_border_lwd,
                           colour = annot_border_col),

        if (is.numeric(annot_df[, nm, drop = T])) {
          ggplot2::scale_fill_viridis_c()
        } else {
          ggplot2::scale_fill_brewer(palette = "Pastel1")
        }
      )
    })

  return(plt_out)
}
