#' Add annotations to ggplot heatmap
#'
#' @keywords internal
#'
#' @param plt `ggplot` object with `geom_tile` layer to add annotations to.
#' @param context Dimension to add annotations to, either "rows" or "columns".
#' @param annot_df Data frame containing the annotations.
#' The first column must contain the labels used in the heatmap rows or columns.
#' Each of the other columns should contain corresponding annotations. The column names are used as labels in the legend.
#' @param annot_pos Positions of annotations. x positions if row annotations, y positions if column annotations.
#' Positions are given by the middle coordinate of the annotation cells.
#' @param annot_size Size of annotations, width of row annotations, height if column annotations.
#' @param annot_border_lwd Linewidth of border lines of annotation cells.
#' @param annot_border_col Colour of border lines of annotation cells.
#' @param annot_border_lty Linetype of border lines of annotation cells.
#' @param draw_legend Logical indicating if a legend should be drawn.
#' @param annot_label Logical indicating if the annotation names should be drawn.
#' @param na_remove Logical indicating if NA values should be removed.
#' @param col_scale Named list of fill scales to use, named after the columns in the annotation data frame.
#' Each element should either be a `scale_fill_*` object or a string specifying a brewer palette or viridis option.
#' @param label_side String specifying which side the labels should be drawn on. "top" or "bottom" for row annotation, "left" or "right" for column annotation.
#' @param label_params A named list of parameters to give `grid::textGrob` to modify annotation label appearance.
#'
#' @return `ggplot` object with added annotations.
#'
add_annotation <- function(plt, context = c("rows", "cols"), annot_df, annot_pos, annot_size,
                           annot_border_lwd = 0.5, annot_border_col = "grey", annot_border_lty = 1,
                           draw_legend = T, annot_label = T, na_remove = F,
                           col_scale = NULL, label_side, label_params = NULL) {
  .names <- .data <- NULL

  annot_names <- colnames(annot_df)[-which(colnames(annot_df) == ".names")]

  plt_out <- plt +
    lapply(annot_names, \(nm) {
      list(
        # Add separate colour scale for each annotation
        ggnewscale::new_scale_fill(),

        # Draw annotation
        ggplot2::geom_tile(data = dplyr::filter(dplyr::select(annot_df, .names, dplyr::all_of(nm)), if (na_remove) !is.na(get(nm)) else T),
                           mapping = ggplot2::aes(x = if (context[1] == "rows") {annot_pos[nm]}
                                                  else {.data[[".names"]]},
                                                  y = if (context[1] == "rows") {.data[[".names"]]}
                                                  else {annot_pos[nm]},
                                                  fill = .data[[nm]]),
                           width = ifelse(context[1] == "rows", annot_size, 1),
                           height = ifelse(context[1] == "rows", 1, annot_size),
                           linewidth = annot_border_lwd, colour = annot_border_col, linetype = annot_border_lty),

        col_scale[[nm]],

        if (annot_label) {
          # Add labels using annotation_custom to add a text grob without disturbing the plot area limits
          ggplot2::annotation_custom(do.call(grid::textGrob, append(list(nm), label_params)),
                                     # Distance between edge of heatmap and labels is 0.2 (default distance of annotation from heatmap)
                                     # (.3 is .2 away from .5 (middle point of cell), .7 is the same distance in the other direction)
                                     xmin = ifelse(context[1] == "rows", annot_pos[nm], ifelse(label_side == "left", .3, nrow(annot_df) + .7)),
                                     xmax = ifelse(context[1] == "rows", annot_pos[nm], ifelse(label_side == "left", .3, nrow(annot_df) + .7)),
                                     ymin = ifelse(context[1] == "rows", ifelse(label_side == "bottom", .3, nrow(annot_df) + .7), annot_pos[nm]),
                                     ymax = ifelse(context[1] == "rows", ifelse(label_side == "bottom", .3, nrow(annot_df) + .7), annot_pos[nm]))
        }
      )
    })

  return(plt_out)
}


#' Prepare annotation parameters and positions
#'
#' @keywords internal
#'
#' @param annot_df Annotation data frame. Should contain the rownames or a column called '.names' with names, other columns are used for annotation
#' @param annot_defaults Default parameters for annotation.
#' @param annot_params Provided annotation parameters to update.
#' @param annot_label_params Annotation label (names next to annotations) parameters to update.
#' @param annot_side String for annotation side.
#' @param context String stating the context ("rows" or "cols").
#' @param annot_label_side Annotation label side.
#' @param data_size Size of data (ncol if row annotations, nrow if column annotations).
#'
#' @returns List with updated annotation parameters, calculated annotation positions, and updated
#' annotation label parameters.
#'
prepare_annotation <- function(annot_df, annot_defaults, annot_params, annot_side, context = c("rows", "cols"),
                               annot_label_params, annot_label_side, data_size) {

  # Logical for annotation position
  if ((annot_side == "left" && context[1] == "rows") ||
      (annot_side == "bottom" && context[1] == "cols")) {
    lannot_side <- T
  } else if ((annot_side == "right" && context[1] == "rows") ||
             (annot_side == "top" && context[1] == "cols")) {
    lannot_side <- F
  } else {
    var_name <- switch(context[1], "rows" = "annot_rows_side", "cols" = "annot_cols_side")
    val_name <- switch(context[1], "rows" = c("left", "right"), "cols" = c("top", "bottom"))
    cli::cli_warn("{.var {var_name}} should be {.val {val_name[1]}} or {.val {val_name[2]}},
                  not {.val {annot_side}}.",
                  class = "annot_side_warn")
    lannot_side <- switch(context[1], "rows" = F, "cols" = T)
  }

  annot_names <- colnames(annot_df)[-which(colnames(annot_df) == ".names")]

  # Replace defaults with any provided options
  annot_params <- replace_default(annot_defaults, annot_params)

  # Get positions of annotations
  annot_pos <- get_annotation_pos(lannot_side, annot_names, annot_params$size,
                                  annot_params$dist, annot_params$gap, data_size)

  # Row annotation label parameters and their defaults (fed to grid::textGrob)
  if (!(context[1] == "rows" && annot_label_side %in% c("top", "bottom")) &&
      !(context[1] == "cols" && annot_label_side %in% c("left", "right"))) {
    side_var <- paste0("annot_", ifelse(context[1] == "rows", "rows", "cols"), "_label_side")
    side_val <- switch(context[1], "rows" = c("top", "bottom"), "cols" = c("left", "right"))
    cli::cli_warn("{.var {side_var}} should be {.val {side_val[1]}} or {.val {side_val[2]}}, not {.val {annot_label_side}}.",
                  class = "annot_label_side_warn")
    annot_label_side <- ifelse(context[1] == "rows", "bottom", "left")
  }

  annot_label_defaults <- list(rot = switch(annot_label_side, "bottom" =, "top" = 90,
                                            "left" =, "right" = 0),
                               just = switch(annot_label_side, "bottom" = "right", "top" = "left",
                                             "left" = "right", "right" = "left"))
  # Also defaults for the gp parameter
  annot_gp_defaults <- grid::gpar(fontsize = 9)
  annot_gp_params <- replace_default(annot_gp_defaults, annot_label_params[["gp"]], add_new = T)
  annot_label_params[["gp"]] <- annot_gp_params
  # Replace defaults and allow for new parameters to be added
  annot_label_params <- replace_default(annot_label_defaults, annot_label_params, add_new = T)

  return(list(annot_df, annot_params, annot_pos, annot_label_params, annot_label_side))
}


#' Calculate positions of annotations for heatmap.
#'
#' @keywords internal
#'
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
get_annotation_pos <- function(annot_side = T, annot_names, annot_size, annot_dist, annot_gap, data_size) {
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


#' Check names of annotation
#'
#' @keywords internal
#'
#' @param annot_df Annotation data frame (annot_rows_df, annot_cols_df).
#' @param names_in Names that exist in the data (rownames, colnames).
#'
#' @returns Annotation data frame (rownames moved to column named '.names' if necessary).
#'
check_annot_df <- function(annot_df, names_in, context) {
  .names <- NULL

  # Check that it's a data frame
  if (!is.data.frame(annot_df)) {
    cli::cli_abort("{.var {context}} must be a {.cls data.frame}, not a {.cls {class(annot_df)}}.")
  }

  # Move names to column if in row names
  if (!".names" %in% colnames(annot_df)) {
    annot_df[[".names"]] <- rownames(annot_df)
    rownames(annot_df) <- NULL
  }

  # Check that annotation contains the correct names
  # If any names don't exist, throw a warning and remove them
  if (any(!annot_df[[".names"]] %in% names_in)) {
    bad_names <- setdiff(annot_df[[".names"]], names_in)
    cli::cli_warn("{?A/Some} name{?s} in the row annotation do{?es/}n't exist in the data: {.val {bad_names}}.",
                   class = "annot_names_warn")
    annot_df <- subset(annot_df, !.names %in% bad_names)
  }

  # Check that there are no duplicate names
  # If there are any, throw and error as it becomes unclear which one is the real value
  if (any(duplicated(annot_df[[".names"]]))) {
    dupl_names <- unique(annot_df[[".names"]][which(duplicated(annot_df[[".names"]]))])
    cli::cli_abort("{.val {dupl_names}} appear{?s/} multiple times in {.var {context}}. Names must be unique.",
                   class = "dupl_annot_name_error")
  }

  return(annot_df)
}
