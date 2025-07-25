#' Make legend order depending on what the plot will contain.
#'
#' @keywords internal
#'
#' @param mode Plotting modes.
#' @param col_scale One or two colour scales (shared for fill and colour). NULL for default, string for Brewer or Viridis, or a scale.
#' @param size_scale Size scales (NULL or ggplot2 scales).
#' @param annot_rows_df Annotation data frame for rows.
#' @param annot_cols_df Annotation data frame for columns.
#' @param legend_order Numeric vector with legend order. NULL for default.
#'
#' @returns A list with aesthetics for the main plot and orders of all legends.
#'
make_legend_order <- function(mode, col_scale = NULL, size_scale = NULL,
                              annot_rows_df = NULL, annot_cols_df = NULL, legend_order = NULL) {

  scales_to_use <- dplyr::case_when(mode %in% c("heatmap", "hm") ~ "fill",
                                    mode %in% c("text") ~ "col",
                                    mode %in% as.character(1:20) ~ "col_size",
                                    mode %in% as.character(21:25) ~ "fill_size",
                                    mode == "none" ~ "none")

  # Remove 'none'
  if ("none" %in% scales_to_use) {scales_to_use <- scales_to_use[-which(scales_to_use == "none")]}

  # Make a vector of the necessary scales for the main plot
  scale_list <- strsplit(scales_to_use, split = "_")
  scale_vec <- unlist(scale_list)

  # Remove scales if only one is needed (e.g. two fill modes but only one scale (or none) provided
  scale_vec <- remove_duplicate_scales(scale_vec, col_scale, size_scale)

  # Assign scales a legend order
  if (!is.null(legend_order) && !all(is.na(legend_order)) && !is.numeric(legend_order)) {
    cli::cli_warn("{.var legend_order} should be NULL (default order), NA (for no legends),
    or a {.cls numeric} vector, not {.cls {class(legend_order)}}.",
                  class = "lgd_order_class_warn")
    legend_order <- NULL
  }

  scale_order <- if (is.null(legend_order)) {
    # Go from 1 to number of legends
    seq_along(scale_vec)
  } else {
    round(legend_order[seq_along(scale_vec)])
  }

  # All annotation scales are fill scales, generate or fetch their order
  if (!is.null(annot_rows_df)) {
    row_annot_order <- if (is.null(legend_order)) {
      # If no order is specified, put them in order
      seq(max(scale_order) + 1, max(scale_order) + ncol(annot_rows_df) - 1)
    } else {
      # Otherwise pick out the indices in order
      legend_order[seq(1, ncol(annot_rows_df) - 1) + length(scale_vec)]
    }
  } else {
    row_annot_order <- NULL
  }

  if (!is.null(annot_cols_df)) {
    col_annot_order <- if (is.null(legend_order)) {
      # Column annotation order also depends on whether row annotation exists or not
      if (!is.null(row_annot_order)) {
        seq(max(row_annot_order) + 1, max(row_annot_order) + ncol(annot_cols_df) - 1)
      } else {
        seq(max(scale_order) + 1, max(scale_order) + ncol(annot_cols_df) - 1)
      }
    } else {
      legend_order[seq(1, ncol(annot_cols_df) - 1) + length(scale_vec) + length(row_annot_order)]
    }
  } else {
    col_annot_order <- NULL
  }

  return(list("main_scales" = list("scales" = scale_vec, "order" = scale_order),
              "row_annot" = row_annot_order, "col_annot" = col_annot_order))

}

#' Remove duplicate scales.
#'
#' If a mixed layout uses the same aesthetic for both triangles and only one (or no) colour or size scale has been specified, remove redundant scales.
#'
#' @keywords internal
#'
#' @param scale_vec Vector of scale aesthetics.
#' @param col_scale Input colour scales (NULL, string or scale object).
#' @param size_scale Input size scales.
#'
#' @returns Vector of aesthetics with duplicates removed if appropriate.
#'
remove_duplicate_scales <- function(scale_vec, col_scale = NULL, size_scale = NULL) {
  for (i in c("fill", "col", "size", "none")) {
    input_scale <- switch(i, "fill" = , "col" = col_scale, "size" = size_scale, "none" = NULL)
    if (sum(scale_vec == i) == 2 &&                                         # Only remove if two of the same name and
        ((length(input_scale) < 2 || all(is.null(unlist(input_scale)))) ||  # input scales not a list with two scales or strings, or
         inherits(input_scale, c("Scale", "ggproto", "gg")))                # a single scale or
    ) {
      scale_vec <- scale_vec[-which(scale_vec == i)[2]]
    }
  }

  return(scale_vec)
}


#' Prepare scales for heatmap.
#'
#' @keywords internal
#'
#' @param scale_order List of necessary scales and their orders, as obtained make_legend_order.
#' @param context Scale context (gghm or ggcorrhm) for deciding which defaults to use.
#' @param val_type String with type of value ('continuous' or 'discrete').
#' @param col_scale Colour scales input.
#' @param col_name Colour scale names.
#' @param size_scale Size scales input.
#' @param size_name Size scale names.
#' @param bins Number of bins to divide scale into (only for ggcorrhm).
#' @param limits Scale limits (for ggcorrhm).
#' @param high Colour at higher end of scale (for ggcorrhm).
#' @param mid Colour at middle point of scale (for ggcorrhm).
#' @param low Colour at Lower end of scale (for ggcorrhm).
#' @param midpoint Middle point of scale (for ggcorrhm).
#' @param size_range Size range for size scale (for ggcorrhm).
#' @param na_col Colour to use for NAs (for ggcorrhm).
#'
#' @returns ggplot2 scales for the plot.
#'
prepare_scales <- function(scale_order, context = c("gghm", "ggcorrhm"), val_type,
                           col_scale = NULL, col_name = "value",
                           size_scale = NULL, size_name = "value",
                           bins = NULL, limits = c(-1, 1),
                           high = "sienna2", mid = "white", low = "skyblue2", midpoint = 0,
                           size_range = NULL, na_col = "grey50") {

  # Input class checks
  check_numeric(bins = bins, allow_null = TRUE, allowed_lengths = 1)
  check_numeric(limits = limits, allow_null = TRUE, allowed_lengths = 2)
  check_numeric(midpoint = midpoint, allow_null = FALSE, allowed_lengths = 1)
  check_numeric(size_range = size_range, allow_null = TRUE, allowed_lengths = c(1, 2))

  # Put scales into lists
  if (is.vector(col_scale)) {
    # For vector, make sure the elements end up in different list indices
    col_scale <- as.list(col_scale)
  } else if (!is.list(col_scale)) {
    col_scale <- list(col_scale)
  }
  if (is.vector(size_scale)) {
    size_scale <- as.list(size_scale)
  } else if (!is.list(size_scale)) {
    size_scale <- list(size_scale)
  }

  main_scales_out <- vector("list", length(scale_order[["main_scales"]][["scales"]]))

  # Keep iterators for different aes types to pick out the correct indices from input scales
  colr_id <- size_id <- 1

  for (i in seq_along(main_scales_out)) {
    # For each required scale, generate default or a specified scale, or use a provided one
    scl <- scale_order[["main_scales"]][["scales"]][[i]]

    legend_order <- scale_order[["main_scales"]][["order"]][[i]]
    input_scale <- switch(scl,
                          "fill" = ,
                          "col" = col_scale[[colr_id]],
                          "size" = size_scale[[size_id]])
    scale_title <- switch(scl,
                          "fill" = ,
                          "col" = col_name[[colr_id]],
                          "size" = size_name[[size_id]])

    if (inherits(input_scale, c("Scale", "ggproto", "gg"))) {
      # If input scale is a scale object, use it as is
      main_scales_out[[i]] <- input_scale

    } else if (is.character(input_scale) && scl != "size") {
      # If a character, check that it's a valid brewer or viridis scale and if so use that
      scl_temp <- get_colour_scale(input_scale, val_type = val_type[1], aes_type = scl, title = scale_title,
                                   limits = limits, bins = bins, leg_order = legend_order, na_col = na_col)

      if (is.null(scl_temp)) {
        # If it didn't work, use default
        main_scales_out[[i]] <- switch(context,
                                       "gghm" = default_scale(val_type[1], scl, legend_order, scale_title, na_col, bins, limits),
                                       "ggcorrhm" = default_scale_corr(scl, bins, limits, size_range, high, mid, low, midpoint, na_col, legend_order, scale_title))
      } else {
        main_scales_out[[i]] <- scl_temp
      }

    } else {
      # Throw a warning if not NULL
      if (!is.null(input_scale)) {
        if (scl == "size") {
          cli::cli_warn("{.var size_scale} should be NULL (default) or a ggplot2 scale object,
          or a combination if a mixed layout.", class = "scale_class_warn")
        } else {
          cli::cli_warn("{.var col_scale} should be NULL (default), a character value,
          or a ggplot2 scale object, or a combination of those if a mixed layout.",
                        class = "scale_class_warn")
        }
      }
      # Otherwise make the default scale
      main_scales_out[[i]] <- switch(context,
                                     "gghm" = default_scale(val_type[1], scl, legend_order, scale_title, na_col, bins, limits),
                                     "ggcorrhm" = default_scale_corr(scl, bins, limits, size_range, high, mid, low, midpoint, na_col, legend_order, scale_title))

    }

    if (scl %in% c("fill", "col")) {colr_id <- colr_id + 1}
    if (scl == "size") {size_id <- size_id + 1}
  }

  return(main_scales_out)
}


#' Get a Brewer or Viridis colour scale.
#'
#' @keywords internal
#'
#' @param name Scale palette/option name.
#' @param val_type Value type ('continuous' or 'discrete').
#' @param aes_type Aesthetic type ('fill' or 'col').
#' @param limits Limits for scale.
#' @param bins Number of bins if binned scale.
#' @param leg_order Order of legend.
#' @param title Legend title.
#'
#' @returns ggplot2 scale using Brewer or Viridis.
#'
get_colour_scale <- function(name, val_type, aes_type, limits = NULL, bins = NULL,
                             leg_order = 1, title = ggplot2::waiver(), na_col = "grey50") {
  # No scale for 'none' mode
  if (aes_type == "none") {
    return(NULL)
  }

  # Set direction of scale
  scl_dir <- 1
  # If the name contains "rev_" or "_rev", reverse the scale
  if (grepl("^rev_|_rev$", name, ignore.case = TRUE)) {
    name <- gsub("^rev_|_rev$", "", name, ignore.case = TRUE)
    scl_dir <- -1
  }

  # Get scale depending on type and name
  # Possible scale names (Brewer palettes, Viridis options)
  brw_pal <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
               "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
               "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn",
               "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
  vir_opt <- c(LETTERS[1:8], c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo"))

  if (!name %in% c(brw_pal, vir_opt)) {
    cli::cli_warn(c("{.val {name}} is not a valid scale. Using default scale instead.",
                    i = "Use NULL for the default, a Brewer or Viridis scale name, or a ggplot2 scale object."),
                  class = "invalid_colr_option_warn")
    return(NULL)
  }

  # Make list of inputs to use for the scale
  # palette/option, limits, guide (for order), maybe bins
  scl_inputs <- list(
    name, limits, title,
    if (is.na(leg_order)) {
      "none"
    } else {
      # Guide is colourbar if continuous, coloursteps if binned continuous, legend if discrete
      if (val_type == "discrete") {
        ggplot2::guide_legend(order = leg_order)
      } else if (val_type == "continuous") {
        if (is.numeric(bins)) {
          ggplot2::guide_coloursteps(order = leg_order)
        } else {
          ggplot2::guide_colourbar(order = leg_order)
        }
      }
    },
    na_col, scl_dir
  )
  names(scl_inputs) <- c(ifelse(name %in% brw_pal, "palette", "option"),
                         "limits", "name", "guide", "na.value", "direction")

  # Make an option for binned continuous scales and add input argument for scale
  if (is.numeric(bins) && val_type == "continuous") {
    aes_type <- paste0(aes_type, "_bin")
    scl_inputs <- append(scl_inputs, list(n.breaks = bins - 1, nice.breaks = ifelse(is.integer(bins), FALSE, TRUE)))
  }

  scl_fun <- if (name %in% brw_pal) {
    # Brewer scales, brewer if discrete, distiller if continuous, fermenter if binned
    switch(val_type,
           "discrete" = switch(aes_type,
                               "fill" = ggplot2::scale_fill_brewer,
                               "col" = ggplot2::scale_colour_brewer),
           "continuous" = switch(aes_type,
                                 "fill" = ggplot2::scale_fill_distiller,
                                 "col" = ggplot2::scale_colour_distiller,
                                 "fill_bin" = ggplot2::scale_fill_fermenter,
                                 "col_bin" = ggplot2::scale_colour_fermenter))

  } else if (name %in% vir_opt) {
    # Viridis scales, viridis_d if discrete, viridis_c if continuous, viridis_b if binned
    switch(val_type,
           "discrete" = switch(aes_type,
                               "fill" = ggplot2::scale_fill_viridis_d,
                               "col" = ggplot2::scale_colour_viridis_d),
           "continuous" = switch(aes_type,
                                 "fill" = ggplot2::scale_fill_viridis_c,
                                 "col" = ggplot2::scale_colour_viridis_c,
                                 "fill_bin" = ggplot2::scale_fill_viridis_b,
                                 "col_bin" = ggplot2::scale_colour_viridis_b))

  }

  # Make the scale
  scl_out <- do.call(scl_fun, scl_inputs)

  return(scl_out)
}


#' Get a default scale for correlation heatmaps.
#'
#' @keywords internal
#'
#' @param aes_type Type of aesthetic ('fill', 'col', or 'size').
#' @param bins Number of bins (for fill and colour scales).
#' @param limits Scale limits (fill and colour).
#' @param size_range Range of sizes (size).
#' @param high Colours at higher end of fill or colour scale.
#' @param mid Colours at middle point of fill or colour scale.
#' @param low Colours at lower end of fill or colour scale.
#' @param midpoint Middle point of colour scale.
#' @param na_col Colour of NAs.
#' @param leg_order Order of legend.
#' @param title Title of legend.
#'
#' @returns ggplot2 scale for correlation heatmap.
#'
default_scale_corr <- function(aes_type, bins = NULL, limits = c(-1, 1), size_range = NULL,
                               high = "sienna2", mid = "white", low = "skyblue2", midpoint = 0,
                               na_col = "grey50", leg_order = 1, title = ggplot2::waiver()) {

  if (aes_type == "size") {
    guide_arg <- if (is.na(leg_order)) {"none"} else {ggplot2::guide_legend(order = leg_order)}
    scale_args <- list(range = size_range, transform = scales::trans_new("abs", abs, abs),
                       guide = guide_arg, name = title)
    scale_fun <- ggplot2::scale_size_continuous

  } else {
    guide_arg <- if (is.na(leg_order)) {"none"} else {ggplot2::guide_colourbar(order = leg_order)}
    scale_args <- list(limits = limits, high = high, mid = mid, low = low, midpoint = midpoint,
                       na.value = na_col, name = title, guide = guide_arg)
    if (is.numeric(bins)) {
      scale_args <- append(scale_args, list(n.breaks = bins - 1, nice.breaks = ifelse(is.integer(bins), FALSE, TRUE)))
      scale_fun <- switch(aes_type,
                          "fill" = ggplot2::scale_fill_steps2,
                          "col" = ggplot2::scale_colour_steps2)
    } else {
      scale_fun <- switch(aes_type,
                          "fill" = ggplot2::scale_fill_gradient2,
                          "col" = ggplot2::scale_colour_gradient2)
    }
  }

  scale_out <- do.call(scale_fun, scale_args)

  return(scale_out)
}


#' Get default scale for non-correlation heatmaps.
#'
#' @keywords internal
#'
#' @param val_type Value type ('continuous' or 'discrete').
#' @param aes_type Aesthetic type ('fill', 'col' or 'size).
#' @param leg_order Order of legend.
#' @param title Legend title.
#'
#' @returns ggplot2 scale for non-correlation heatmaps.
#'
default_scale <- function(val_type, aes_type, leg_order = 1, title = ggplot2::waiver(),
                          na_col = "grey50", bins = NULL, limits = NULL) {

  if (is.numeric(bins) && aes_type != "size" && val_type == "continuous") {
    aes_type <- paste0(aes_type, "_bins")
  }

  # Use the default ggplot2 scale but set the order
  scale_fun <- switch(
    val_type,
    "continuous" = switch(
      aes_type,
      "fill" = ggplot2::scale_fill_continuous,
      "col" = ggplot2::scale_colour_continuous,
      "size" = ggplot2::scale_size_continuous,
      "fill_bins" = ggplot2::scale_fill_binned,
      "col_bins" = ggplot2::scale_colour_binned
    ),
    "discrete" = switch(
      aes_type,
      "fill" = ggplot2::scale_fill_discrete,
      "col" = ggplot2::scale_colour_discrete,
      "size" = ggplot2::scale_size_discrete
    )
  )

  scale_args <- list(
    name = title,
    guide = if (is.na(leg_order)) {
      "none"
    } else {
      if (aes_type == "size") {ggplot2::guide_legend(order = leg_order)}
      else if (val_type == "continuous") {ggplot2::guide_colourbar(order = leg_order)}
      else if (val_type == "discrete") {ggplot2::guide_legend(order = leg_order)}
    }
  )
  if (aes_type != "size") {
    # Add NA colour if not a size scale
    scale_args[["na.value"]] <- na_col

    if (val_type == "continuous") {
      # Add limits if not discrete
      scale_args[["limits"]] <- limits

      if (grepl("_bins", aes_type)) {
        # Finally add bins if binned
        scale_args <- append(scale_args, list(n.breaks = bins - 1, nice.breaks = ifelse(is.integer(bins), FALSE, TRUE)))
      }
    }
  }

  scale_out <- do.call(scale_fun, scale_args)

  return(scale_out)
}


#' Pick out relevant scales and format for mixed layout.
#'
#' @keywords internal
#'
#' @param main_scales Scales for main plot as obtained from prepare_scales.
#' @param scale_order List with order of scales as obtained from make_legend_order.
#' @param aes_type Aesthetic for which to pick out scales.
#' @param layout Layout of heatmap.
#'
#' @returns List of scales to use.
#'
extract_scales <- function(main_scales, scale_order, aes_type, layout) {
  if (!is.list(main_scales)) {main_scales <- list(main_scales)}

  scale_list <- main_scales[scale_order[["main_scales"]][["scales"]] %in% aes_type]
  if (length(scale_list) < 1) {
    scale_list <- NULL
  }
  # If only one scale made or provided, return a list with NULL for the second element
  if (length(layout) == 2 & length(scale_list) < 2) {
    scale_list <- list(scale_list, NULL)
  }

  return(scale_list)
}


#' Prepare default colour scales for annotation.
#'
#' Prepares a brewer palette or viridis option for all annotations that don't have any colour scale
#' specified by the user. There are eight options each for brewer (categorical) and viridis (continuous) and they are selected
#' sequentially, going back to the beginning if there are more than eight annotations of each kind.
#'
#' @keywords internal
#'
#' @param scale_order List containing orders of scales as obtained from make_legend_order.
#' @param annot_rows_df Data frame with annotation for rows.
#' @param annot_cols_df Data frame with annotation for columns.
#' @param annot_rows_col List with colour scales for rows.
#' @param annot_cols_col List with colour scales for columns.
#'
#' @returns List of length two containing lists of row annotation and column annotation.
#'
prepare_scales_annot <- function(scale_order, annot_rows_df = NULL, annot_cols_df = NULL,
                                 annot_rows_col = NULL, annot_cols_col = NULL, na_col = "grey50") {

  # Check NA colour
  if (is.null(na_col)) {
    cli::cli_abort("{.var annot_na_col} must be a colour, not NULL.",
                   class = "annot_na_col_length_error")
  }

  # Go through row and then column annotations and assign a scale if not provided
  disc_num <- 1
  cont_num <- 1

  lst_in <- list(list(annot_rows_df, annot_rows_col), list(annot_cols_df, annot_cols_col))
  # Get scale orders
  leg_orders <- list(scale_order[["row_annot"]], scale_order[["col_annot"]])

  lst_out <- vector("list", 2)
  names(lst_out) <- c("rows", "cols")

  for (i in seq_along(lst_in)) {
    # Get annotation names to compare if corresponding names are provided in colour scales list
    annot_nm <- colnames(lst_in[[i]][[1]])[-which(colnames(lst_in[[i]][[1]]) == ".names")]

    # Skip ahead if no annotations
    if (is.null(annot_nm)) {next()}

    scl_nm <- names(lst_in[[i]][[2]])

    # If no list provided, make one
    # If not all names are accounted for, make a list that has all
    if (!all(annot_nm %in% scl_nm)) {
      lst_in_new <- vector("list", length(annot_nm))
      names(lst_in_new) <- annot_nm
      # Fill in existing values
      lst_in_new[scl_nm] <- lst_in[[i]][[2]]
      lst_in[[i]][[2]] <- lst_in_new
    }

    # Go through names and if no colour scale is provided, assign one
    # Increment counters depending on type to provide new colour scales (up to 8 per type, then start from beginning)
    for (j in annot_nm) {
      # Get data type from annotation data frame
      type <- if (is.character(lst_in[[i]][[1]][[j]]) |
                  is.factor(lst_in[[i]][[1]][[j]]) |
                  is.logical(lst_in[[i]][[1]][[j]])) {
        "discrete"
      } else {
        "continuous"
      }
      # Get legend order
      lg_ord <- leg_orders[[i]][[which(annot_nm == j)]]

      # Create scale based on input scale type
      if (is.character(lst_in[[i]][[2]][[j]])) {
        # Input is a string, get the corresponding brewer or viridis scale
        lst_in[[i]][[2]][[j]] <- get_colour_scale(lst_in[[i]][[2]][[j]], type, "fill",
                                                  leg_order = lg_ord, na_col = na_col)

        # If NULL is returned, get a default
        if (is.null(lst_in[[i]][[2]][[j]])) {
          if (type == "discrete") {
            scl_name <- get_default_annot_scale(disc_num, type)
            disc_num <- increment1to8(disc_num)

          } else if (type == "continuous") {
            scl_name <- get_default_annot_scale(cont_num, type)
            cont_num <- increment1to8(cont_num)

          }

          lst_in[[i]][[2]][[j]] <- get_colour_scale(scl_name, type, "fill", leg_order = lg_ord, na_col = na_col)
        }
      } else if (inherits(lst_in[[i]][[2]][[j]], c("Scale", "ggproto", "gg"))) {
        # Keep scale if provided
        next()

      } else {
        if (!is.null(lst_in[[i]][[2]][[j]])) {
          annot_dim <- switch(i, "1" = "row", "2" = "column")
          cli::cli_warn(c("The input for {.var {j}} in the {annot_dim} annotation colours is not recognised. Using default colour scale.",
                        i = "Recognised formats are NULL (default), {.cls character} values, and {.var ggplot2} scale objects."),
                        class = "annot_fill_class_warn")
        }

        # Any other input uses default, increment counter
        if (type == "discrete") {
          scl_name <- get_default_annot_scale(disc_num, type)
          disc_num <- increment1to8(disc_num)

        } else if (type == "continuous") {
          scl_name <- get_default_annot_scale(cont_num, type)
          cont_num <- increment1to8(cont_num)

        }

        lst_in[[i]][[2]][[j]] <- get_colour_scale(scl_name, type, "fill", leg_order = lg_ord, na_col = na_col)
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
#' @param type String specifying data type (discrete or continuous).
#'
#' @returns Brewer or Viridis ggplot2 scale name.
#'
get_default_annot_scale <- function(num, type) {

  # Pick out scales from a predetermined list and order
  name <- if (type == "discrete") {
    c("Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Paired", "Dark2", "Accent")[num]
  } else if (type == "continuous") {
    c("D", "A", "G", "C", "E", "B", "FALSE", "H")[num]
  }

  return(name)
}

