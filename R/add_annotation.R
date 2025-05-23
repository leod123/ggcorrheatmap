#' Calculate positions of annotations for heatmap.
#'
#' @keywords internal
#'
#' @param annot_dim Dimension to plot annotations along (rows or columns).
#' @param annot_side Logical specifying annotation position. TRUE is left of the heatmap if row annotation,
#' bottom of heatmap if column annotation
#' @param annot_names Names of the annotations.
#' @param annot_size Size of annotation cells where 1 is the size of a heatmap cell.
#' @param annot_dist Distance between heatmap and first annotation.
#' @param annot_gap Size of gap between annotations.
#' @param data_size Number of rows or columns in the main heatmap (which is symmetric). Used to get
#' starting position of annotations.
#'
#' @return Numeric vector of annotation cell positions
#'
get_annotation_pos <- function(annot_dim = c("rows", "cols"), annot_side = T, annot_names,
                               annot_size, annot_dist, annot_gap, data_size) {
  positions <- sapply(seq_along(annot_names), \(id) {
    ifelse(annot_side, 1, data_size) +            # To middle of first or last column/row
      (0.5 + annot_dist +                         # Add half of cell width + specified distance to heatmap
         0.5 * annot_size +                       # Add half of annotation width (cell middle point is the coordinate for the cell)
         annot_size * (id - 1) +                  # Add width of previous annotations
         annot_gap * (id - 1)) *                  # Add gap between annotations
      ifelse(annot_side, -1, 1)                   # Subtract if left side, add if right side
  }, simplify = T)

  names(positions) <- annot_names

  return(positions)
}

#' Add annotations to ggplot heatmap
#'
#' @keywords internal
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
#' @param col_scale Named list of fill scales to use, named after the columns in the annotation data frame.
#' Each element should either be a `scale_fill_*` object or a string specifying a brewer palette or viridis option.
#'
#' @return `ggplot` object with added annotations.
#'
add_annotation <- function(ggp, annot_dim = c("rows", "cols"), annot_df, annot_pos, annot_size,
                           annot_border_lwd = 0.5, annot_border_col = "grey", draw_legend = T,
                           col_scale = NULL, legend_order = NULL) {

  annot_names <- colnames(annot_df)[-1]
  # Vector for determining the order in which to draw the annotation legends
  if (is.null(legend_order)) {
    legend_order <- seq_along(annot_names) + 1
    names(legend_order) <- annot_names
  }

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

#' Prepare default colour scales for annotation.
#'
#' @keywords internal
#'
#' @param annot_rows_df Data frame with annotation for rows.
#' @param annot_cols_df Data frame with annotation for columns.
#' @param annot_rows_col List with colour scales for rows.
#' @param annot_cols_col List with colour scales for columns.
#'
#' @returns List of length two containing lists of row annotation and column annotation.
#'
prepare_annot_col <- function(annot_rows_df = NULL, annot_cols_df = NULL,
                              annot_rows_col = NULL, annot_cols_col = NULL) {

  # Go through row and then column annotations and assign a scale if not provided
  cat_num <- 1
  cont_num <- 1

  lst_in <- list(list(annot_rows_df, annot_rows_col), list(annot_cols_df, annot_cols_col))
  lst_out <- vector("list", 2)
  for (i in seq_along(lst_in)) {
    # Get annotation names to compare if corresponding names are provided in colour scales list
    annot_nm <- colnames(lst_in[[i]][[1]])[-1]
    scl_nm <- names(lst_in[[i]][[2]])

    # If no list provided, make one
    if (!is.list(lst_in[[i]][[2]])) {
      lst_in[[i]][[2]] <- vector("list", length(annot_nm))
    }

    # Go through names and if no colour scale is provided, assign one
    # Increment counters depending on type to provide new colour scales (up to 8 per type, then start from beginning)
    for (j in annot_nm) {
      if (!j %in% scl_nm & is.numeric(lst_in[[i]][[1]][[j]])) {
        lst_in[[i]][[2]][[j]] <- get_colour_scale(cont_num, "numeric")
        cont_num <- increment1to8(cont_num)

      } else if (!j %in% scl_nm & (is.character(lst_in[[i]][[1]][[j]]) | is.factor(lst_in[[i]][[1]][[j]]))) {
        lst_in[[i]][[2]][[j]] <- get_colour_scale(cat_num, "character")
        cat_num <- increment1to8(cat_num)
      }
    }

    lst_out[[i]] <- lst_in[[i]][[2]]
  }

  return(lst_out)
}

#' Increment between 1 and 8.
#'
#' @keywords internal
#'
#' @param x Integer to increment.
#'
#' @returns Integer. x + 1 if x is between 1 and 7, 1 otherwise.
#'
increment1to8 <- function(x) {
  # Increment between 1 and 8
  if (x >= 8 | x < 1) return(1L)
  else return(x + 1L)
}

#' Colour scale dispenser.
#'
#' @keywords internal
#'
#' @param num Integer between 1 and 8.
#' @param type String specifying class.
#'
#' @returns String with brewer or viridis scale.
#'
get_colour_scale <- function(num, type) {

  # Pick out scales from a predetermined list and order
  if (type %in% c("character", "factor")) {
    scl_out <- c("Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Paired", "Dark2", "Accent")[num]
  } else if (type %in% c("numeric", "double", "integer")) {
    scl_out <- c("D", "A", "G", "C", "E", "B", "F", "H")[num]
  }

  return(scl_out)
}
