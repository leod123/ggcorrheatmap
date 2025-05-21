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
#' @param draw_legend Logical indicating if a legend should be drawn.
#' @param col_scale Colour scale for filling cells. Either a `ggplot2` scale object or a string specifying the brewer or viridis scale.
#'
#' @return `ggplot` object with added annotations.
#' @export
#'
#' @examples
add_annotation <- function(ggp, annot_dim = c("rows", "cols"), annot_df, annot_pos, annot_size,
                           annot_border_lwd = 0.5, annot_border_col = "grey", draw_legend = T,
                           col_scale = NULL) {

  annot_names <- colnames(annot_df)[-1]
  # Vector for determining the order in which to draw the annotation legends
  legend_order <- seq_along(annot_names) + 1
  names(legend_order) <- annot_names

  # Annotation colours
  # If provided colour scales are not in a list, generate a placeholder list to use default colours
  if (!is.list(col_scale)) {
    col_scale <- list()

  }
  # Check that it contains the names of the annotations, otherwise fill in with missing names
  col_scale[annot_names[!annot_names %in% names(col_scale)]] <- lapply(seq_along(annot_names[!annot_names %in% names(col_scale)]), function(x) NULL)

  plt_out <- ggp +
    lapply(annot_names, \(nm) {
      list(
        # Add separate colour scale for each annotation
        ggnewscale::new_scale_fill(),

        # Draw annotation
        ggplot2::geom_tile(data = annot_df,
                           mapping = ggplot2::aes(x = if (annot_dim[1] == "rows") {annot_pos[nm]}
                                                  else {.data[[colnames(annot_df)[1]]]},
                                                  y = if (annot_dim[1] == "rows") {.data[[colnames(annot_df)[1]]]}
                                                  else {annot_pos[nm]},
                                                  fill = .data[[nm]]),
                           width = ifelse(annot_dim[1] == "rows", annot_size, 1),
                           height = ifelse(annot_dim[1] == "rows", 1, annot_size),
                           linewidth = annot_border_lwd,
                           colour = annot_border_col),

        # Getting colour scales for filling cells
        if (is.character(col_scale[[nm]])) {
          # Use either as discrete or continuous scale
          if (is.character(annot_df[[nm]]) | is.factor(annot_df[[nm]])) {
            ggplot2::scale_fill_brewer(palette = col_scale[[nm]], guide = if (draw_legend) ggplot2::guide_legend(order = legend_order[nm]) else "none")
          } else {
            ggplot2::scale_fill_viridis_c(option = col_scale[[nm]], guide = if (draw_legend) ggplot2::guide_colourbar(order = legend_order[nm]) else "none")
          }

        } else if ("Scale" %in% class(col_scale[[nm]])) {
          # Use provided scale. Legend drawing order is then left to the user
          col_scale[[nm]]

        } else {
          # If no colour scale is provided at all
          if (is.character(annot_df[[nm]]) | is.factor(annot_df[[nm]])) {
            ggplot2::scale_fill_brewer(palette = "Pastel1", guide = if (draw_legend) ggplot2::guide_legend(order = legend_order[nm]) else "none")
          } else {
            ggplot2::scale_fill_viridis_c(guide = if (draw_legend) ggplot2::guide_colourbar(order = legend_order[nm]) else "none")
          }
        }
      )
    })

  return(plt_out)
}
