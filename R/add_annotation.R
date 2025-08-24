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
#' @param show_annot_names Logical indicating if the annotation names should be drawn.
#' @param na_remove Logical indicating if NA values should be removed.
#' @param col_scale Named list of fill scales to use, named after the columns in the annotation data frame.
#' Each element should either be a `scale_fill_*` object or a string specifying a brewer palette or viridis option.
#' @param names_side String specifying which side the labels should be drawn on. "top" or "bottom" for row annotation, "left" or "right" for column annotation.
#' @param name_params A named list of parameters to give to `ggplot2::geom_text` or `grid::textGrob` to modify annotation label appearance.
#' @param names_strategy String stating which strategy to use for drawing names (geom, grid).
#'
#' @return `ggplot` object with added annotations.
#'
add_annotation <- function(plt, context = c("rows", "cols"), annot_df, annot_pos, annot_size,
                           annot_border_lwd = 0.5, annot_border_col = "grey", annot_border_lty = 1,
                           show_annot_names = TRUE, na_remove = FALSE, col_scale = NULL, names_side,
                           name_params = NULL, names_strategy = "geom") {
  .names <- .data <- name <- x <- y <- NULL

  check_logical(annot_na_remove = na_remove)
  check_logical(show_annot_names = show_annot_names)

  annot_names <- colnames(annot_df)[-which(colnames(annot_df) %in% c(".names", ".row_facets", ".col_facets"))]

  if (isTRUE(show_annot_names)) {
    annot_name_df <- prepare_annot_label(annot_df, annot_pos, names_side, context)
  }

  # Include facet names if there are any
  facet_names <- c(".row_facets", ".col_facets")[which(c(".row_facets", ".col_facets") %in% colnames(annot_df))]

  plt_out <- plt +
    lapply(annot_names, function(nm) {
      list(
        # Add separate colour scale for each annotation
        ggnewscale::new_scale_fill(),

        # Draw annotation
        ggplot2::geom_tile(data = dplyr::filter(dplyr::select(annot_df, .names, dplyr::all_of(nm), dplyr::all_of(facet_names)),
                                                if (na_remove) !is.na(get(nm)) else TRUE),
                           mapping = ggplot2::aes(x = if (context[1] == "rows") {annot_pos[nm]}
                                                  else {.data[[".names"]]},
                                                  y = if (context[1] == "rows") {.data[[".names"]]}
                                                  else {annot_pos[nm]},
                                                  fill = .data[[nm]]),
                           width = ifelse(context[1] == "rows", annot_size, 1),
                           height = ifelse(context[1] == "rows", 1, annot_size),
                           linewidth = annot_border_lwd, colour = annot_border_col, linetype = annot_border_lty),

        col_scale[[nm]],

        if (show_annot_names && names_strategy == "geom") {
          do.call(ggplot2::geom_text,
                  append(list(data = subset(annot_name_df, name == nm),
                              mapping = ggplot2::aes(x = x, y = y, label = name)),
                         name_params))
        } else {NULL},

        if (show_annot_names && names_strategy == "grid") {
          # Add labels using annotation_custom to add a text grob without disturbing the plot area limits
          ggplot2::annotation_custom(tryCatch(do.call(grid::textGrob, append(list(nm), name_params)),
                                              error = function(err) {
                                                cli::cli_abort(c("Error in grid::textGrob when drawing annotation names:",
                                                                 i = err[["message"]]),
                                                               # Make the call a bit more informative
                                                               call = call("add_annotation"),
                                                               class = "grid_textgrob_error")
                                              }),
                                     # Distance between edge of heatmap and labels is 0.2 (default distance of annotation from heatmap)
                                     # (.3 is .2 away from .5 (middle point of cell), .7 is the same distance in the other direction)
                                     xmin = ifelse(context[1] == "rows", annot_pos[nm], ifelse(names_side == "left", .3, nrow(annot_df) + .7)),
                                     xmax = ifelse(context[1] == "rows", annot_pos[nm], ifelse(names_side == "left", .3, nrow(annot_df) + .7)),
                                     ymin = ifelse(context[1] == "rows", ifelse(names_side == "bottom", .3, nrow(annot_df) + .7), annot_pos[nm]),
                                     ymax = ifelse(context[1] == "rows", ifelse(names_side == "bottom", .3, nrow(annot_df) + .7), annot_pos[nm]))
        } else {NULL}
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
#' @param annot_names_size Size of annotation names.
#' @param annot_name_params Annotation label (names next to annotations) parameters to update.
#' @param annot_side String for annotation side.
#' @param context String stating the context ("rows" or "cols").
#' @param annot_names_side Annotation label side.
#' @param data_size Size of data (ncol if row annotations, nrow if column annotations).
#' @param x_long Long format plotting data, to see if there are any facets to take into account.
#' @param annot_names_strategy String stating how to draw names (geom, grid). To deal with deprecated argument.
#'
#' @returns List with updated annotation parameters, calculated annotation positions, and updated
#' annotation label parameters.
#'
prepare_annotation <- function(annot_df, annot_defaults, annot_params, annot_side, context = c("rows", "cols"),
                               annot_names_size, annot_name_params, annot_names_side, data_size, x_long,
                               annot_names_strategy = "geom") {
  .row_facets <- .col_facets <- NULL

  # Get data size for annotation positions (will depend on facetting)
  if (context[1] == "rows") {
    # If there are column facets, the edge of the heatmap will not be at the full data size
    # but only for the number of columns in the last facet
    if (".col_facets" %in% colnames(x_long)) {
      last_facet <- levels(x_long[[".col_facets"]])[length(levels(x_long[[".col_facets"]]))]
      data_size <- length(unique(subset(x_long, .col_facets == last_facet)[["col"]]))

    } else {
      # If no facetting, the heatmap edge is simply at ncol of the data
      data_size <- length(unique(x_long[["col"]]))
    }
  } else {
    # For column annotations, use the first facet instead
    if (".row_facets" %in% colnames(x_long)) {
      first_facet <- levels(x_long[[".row_facets"]])[1]
      data_size <- length(unique(subset(x_long, .row_facets == first_facet)[["row"]]))
    } else {
      data_size <- length(unique(x_long[["row"]]))
    }
  }

  # Check that annotation and label parameters are in lists
  if (!is.list(annot_params) && !is.null(annot_params)) {
    var_name <- paste0("annot_", context[1], "_params")
    cli::cli_warn("{.var {var_name}} should be a {.cls list} or NULL, not {.cls {class(annot_params)}}.",
                  class = "annot_params_warn")
  }
  if (!is.list(annot_name_params) && !is.null(annot_name_params)) {
    var_name <- paste0("annot_", context[1], "_names_side")
    cli::cli_abort("{.var {var_name}} must be a {.cls list}, not {.cls {class(annot_name_params)}}.",
                   class = "annot_name_params_class_error")
  }

  # Logical for annotation position
  if ((annot_side == "left" && context[1] == "rows") ||
      (annot_side == "bottom" && context[1] == "cols")) {
    lannot_side <- TRUE
  } else if ((annot_side == "right" && context[1] == "rows") ||
             (annot_side == "top" && context[1] == "cols")) {
    lannot_side <- FALSE
  } else {
    var_name <- paste0("annot_", context[1], "_side")
    val_name <- switch(context[1], "rows" = c("left", "right"), "cols" = c("top", "bottom"))
    cli::cli_warn("{.var {var_name}} should be {.val {val_name[1]}} or {.val {val_name[2]}},
                  not {.val {annot_side}}.",
                  class = "annot_side_warn")
    lannot_side <- switch(context[1], "rows" = FALSE, "cols" = TRUE)
  }

  annot_names <- colnames(annot_df)[-which(colnames(annot_df) == ".names")]

  # Replace defaults with any provided options
  annot_params <- replace_default(annot_defaults, annot_params,
                                  warning_context = paste0("In {.var annot_", context[1], "_params}: "))

  # Check final parameters
  if (any(c(!is.numeric(annot_params[["dist"]]), !is.numeric(annot_params[["gap"]]), !is.numeric(annot_params[["size"]]))) ||
      any(c(length(annot_params[["dist"]]) > 1, length(annot_params[["gap"]]) > 1, length(annot_params[["size"]]) > 1))) {
    non_num_var <- c("annot_dist", "annot_gap", "annot_size")[
      which(sapply(list(annot_params[["dist"]], annot_params[["gap"]], annot_params[["size"]]), function(x) {
        !is.numeric(x) || length(x) > 1
      }))
    ]
    cli::cli_abort("{.var {non_num_var}} must be {.cls numeric} with length one.",
                   class = "annot_nonnum_error")
  }

  # Get positions of annotations
  annot_pos <- get_annotation_pos(lannot_side, annot_names, annot_params$size,
                                  annot_params$dist, annot_params$gap, data_size)

  # Throw a warning for invalid name side values
  if (!(context[1] == "rows" && annot_names_side %in% c("top", "bottom")) &&
      !(context[1] == "cols" && annot_names_side %in% c("left", "right"))) {
    side_var <- paste0("annot_", context[1], "_names_side")
    side_val <- switch(context[1], "rows" = c("top", "bottom"), "cols" = c("left", "right"))
    cli::cli_warn("{.var {side_var}} should be {.val {side_val[1]}} or {.val {side_val[2]}}, not {.val {annot_names_side}}.",
                  class = "annot_names_side_warn")
    annot_names_side <- ifelse(context[1] == "rows", "bottom", "left")
  }

  # Set default label parameters
  if (annot_names_strategy == "geom") {
    annot_names_defaults <- list(size = annot_names_size,
                                 angle = switch(annot_names_side, "bottom" =, "top" = 90,
                                                "left" =, "right" = 0),
                                 hjust = switch(annot_names_side, "bottom" =, "left" = 1,
                                                "top" =, "right" = 0))
    annot_name_params <- replace_default(annot_names_defaults, annot_name_params, add_new = TRUE)

  } else if (annot_names_strategy == "grid") {
    annot_names_defaults <- list(rot = switch(annot_names_side, "bottom" =, "top" = 90,
                                              "left" =, "right" = 0),
                                 just = switch(annot_names_side, "bottom" = "right", "top" = "left",
                                               "left" = "right", "right" = "left"))
    # Also defaults for the gp parameter
    check_numeric(annot_names_size = annot_names_size, allow_null = FALSE, allowed_lengths = 1)
    annot_gp_defaults <- grid::gpar(fontsize = annot_names_size)
    annot_gp_params <- replace_default(annot_gp_defaults, annot_name_params[["gp"]], add_new = TRUE)
    annot_name_params[["gp"]] <- annot_gp_params
    # Replace defaults and allow for new parameters to be added
    annot_name_params <- replace_default(annot_names_defaults, annot_name_params, add_new = TRUE)

  }


  # If facet columns exist in data, add them to the annotation too
  # For row annotation, add row facets by matching rownames
  # also add the last column facet to only have the annotation in the last facet
  # (and vice versa for column annotation)
  if (".row_facets" %in% colnames(x_long)) {
    if (context[1] == "rows") {
      annot_df <- dplyr::left_join(
        annot_df,
        dplyr::distinct(x_long[, c("row", ".row_facets")]),
        by = c(".names" = "row")
      )

    } else {
      # If rows are facetted and the column annotation should be at the top, the annotations must
      # be put in the first facet, otherwise it will stretch the last facet
      if (isFALSE(lannot_side)) {
        annot_df[[".row_facets"]] <- as.character(levels(x_long[[".row_facets"]])[1])
      } else {
        annot_df[[".row_facets"]] <- as.character(levels(x_long[[".row_facets"]])[length(levels(x_long[[".row_facets"]]))])
      }
    }
  }

  if (".col_facets" %in% colnames(x_long)) {
    if (context[1] == "cols") {
      annot_df <- dplyr::left_join(
        annot_df,
        dplyr::distinct(x_long[, c("col", ".col_facets")]),
        by = c(".names" = "col")
      )

    } else {
      if (isTRUE(lannot_side)) {
        annot_df[[".col_facets"]] <- as.character(levels(x_long[[".col_facets"]])[1])
      } else {
        annot_df[[".col_facets"]] <- as.character(levels(x_long[[".col_facets"]])[length(levels(x_long[[".col_facets"]]))])
      }
    }
  }

  return(list(annot_df, annot_params, annot_pos, annot_name_params, annot_names_side))
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
get_annotation_pos <- function(annot_side = TRUE, annot_names, annot_size, annot_dist, annot_gap, data_size) {
  positions <- sapply(seq_along(annot_names), function(id) {
    ifelse(annot_side, 1, data_size) +            # To middle of first or last column/row
      (0.5 + annot_dist +                         # Add half of cell width + specified distance to heatmap
         0.5 * annot_size +                       # Add half of annotation width (cell middle point is the coordinate for the cell)
         annot_size * (id - 1) +                  # Add width of previous annotations
         annot_gap * (id - 1)) *                  # Add gap between annotations
      ifelse(annot_side, -1, 1)                   # Subtract if left side, add if right side
  }, simplify = TRUE)

  names(positions) <- annot_names

  return(positions)
}


prepare_annot_label <- function(annot_df, annot_pos, names_side, context = c("rows", "cols")) {

  df_out <- if (context[1] == "rows") {
    df_temp <- data.frame(name = names(annot_pos),
                          x = annot_pos,
                          # Put at the end + a bit outside
                          y = ifelse(names_side == "bottom", .3, nrow(annot_df) + .7))
    # Add facetting columns
    # For row facets, put the labels only in the first or last facet depending on the label side
    # Also modify y positions to take into account that the facet may contain fewer rows than the whole data
    if (".row_facets" %in% colnames(annot_df)) {
      df_temp[["y"]] <- ifelse(names_side == "bottom", .3,
                               sum(annot_df[[".row_facets"]] == levels(annot_df[[".row_facets"]])[1]) + .7)
      df_temp[[".row_facets"]] <- ifelse(names_side == "bottom",
                                         as.character(levels(annot_df[[".row_facets"]])[length(levels(annot_df[[".row_facets"]]))]),
                                         as.character(levels(annot_df[[".row_facets"]])[1]))
    }
    # For col facets, use the same as the annotation uses
    if (".col_facets" %in% colnames(annot_df)) {
      df_temp[[".col_facets"]] <- unique(annot_df[[".col_facets"]])
    }

    df_temp

  } else if (context[1] == "cols") {
    df_temp <- data.frame(name = names(annot_pos),
                          x = ifelse(names_side == "left", .3, nrow(annot_df) + .7),
                          y = annot_pos)

    if (".row_facets" %in% colnames(annot_df)) {
      df_temp[[".row_facets"]] <- unique(annot_df[[".row_facets"]])
    }

    if (".col_facets" %in% colnames(annot_df)) {
      df_temp[["x"]] <- ifelse(names_side == "left", .3,
                               sum(annot_df[[".col_facets"]] == levels(annot_df[[".col_facets"]])[length(levels(annot_df[[".col_facets"]]))]) + .7)
      df_temp[[".col_facets"]] <- ifelse(names_side == "left",
                                         as.character(levels(annot_df[[".col_facets"]])[1]),
                                         as.character(levels(annot_df[[".col_facets"]])[length(levels(annot_df[[".col_facets"]]))]))
    }

    df_temp
  }

  return(df_out)
}

