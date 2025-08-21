#' Prepare facetting columns in the plotting data.
#'
#' @keywords internal
#'
#' @param x_long Long format plotting data.
#' @param x Wide format plotting data.
#' @param facet_in Facetting user input (vector or data frame).
#' @param layout Plot layout vector.
#' @param context Context of facetting (row or col).
#' @param dendro List containing clustering results (or NULL if no clustering).
#'
#' @returns Long format plotting data with facetting column added.
#'
prepare_facets <- function(x_long, x, facet_in, layout, context = c("row", "col"), dendro = NULL) {
  nm <- switch(context[1], "row" = rownames(x), "col" = colnames(x))

  # If data has been clustered, use the cluster labels as facetting memberships
  # and only accept a single numeric value (k) for the facet input
  if (!is.null(dendro)) {
    if (length(facet_in) > 1 || !is.numeric(facet_in)) {
      cli::cli_abort("If {paste0(context, ifelse(context[1] == 'col', 'umns', 's'))} are clustered,
                     {.var {paste0('facet_', context[1], 's')}} must be a single numeric value (for
                     the number of clusters to split into).",
                     class = "facet_clust_error")
    }

    # Get named cluster labels
    facet_in <- cutree(dendro$clust, facet_in)

  }

  if (is.numeric(facet_in) && is.atomic(facet_in) &&
      length(facet_in) < length(unique(x_long[[context[1]]]))) {
    # If named, ignore with a warning
    if (!is.null(names(facet_in))) {
      cli::cli_warn("To match names, {.var {paste0('facet_', context[1], 's')}} must be the same length
                    as the number of {paste0(context[1], ifelse(context[1] == 'col', 'umn', ''), 's')}.
                    Using the numbers as indices for the gaps.", class = "facet_names_warn")
    }

    # If a numeric vector shorter than number of unique rows or cols, use as indices to create facets
    facet_df <- data.frame(nm, make_facet_vector(facet_in, length(nm)))

  } else if (is.atomic(facet_in) && length(facet_in) == length(unique(x_long[[context[1]]]))) {

    # If a vector the same length as rows or cols, use directly as facets if they are not named
    # If named, match with the rows/cols
    if (!is.null(names(facet_in))) {
      # If all names are there, match them with the facets
      if (all(nm %in% names(facet_in))) {
        facet_df <- data.frame(nm, facet_in[match(nm, names(facet_in))])

      } else {
        # Otherwise throw a warning and use facets without matching names
        cli::cli_warn("All names must be present if {.var {paste0('facet_', context[1], 's')}} has names.
                      The {paste0(context[1], ifelse(context[1] == 'col', 'umn', ''), 's')} will be facetted directly without matching names.",
                      class = "facet_names_warn")
        facet_df <- data.frame(nm, facet_in)
      }

    } else {
      # If no names, use as is
      facet_df <- data.frame(nm, facet_in)
    }

  } else {
    cli::cli_abort("{.var {paste0('facet_', context[1], 's')}} must be either a vector with indices to split at,
                     or a vector of facet memberships (may be named for matching).",
                   class = "invalid_facet_input_error")
  }

  facet_name <- paste0(".", context[1], "_facets")
  colnames(facet_df) <- c(context[1], facet_name)
  # Turn matching variable into factor to not lose factor levels of x_long
  facet_df[[context[1]]] <- factor(facet_df[[context[1]]], levels = levels(x_long[[context[1]]]))

  x_long <- dplyr::left_join(x_long, facet_df, by = context[1])

  # If facet variable is not a factor, turn it into one with levels in order of appearance
  # Reverse order of row facets if layout uses a top left and/or bottom right component
  # (otherwise the reversed y axis will mess things up)
  if (!is.factor(x_long[[facet_name]])) {
    lvls <- unique(x_long[[facet_name]])
  } else {
    lvls <- levels(x_long[[facet_name]])
  }

  if (context[1] == "row" && any(c("tl", "topleft", "br", "bottomright") %in% layout)) {
    lvls <- rev(lvls)
  }

  x_long[[facet_name]] <- factor(x_long[[facet_name]], levels = lvls)

  return(x_long)
}

#' Make vector for facetting
#'
#' @keywords internal
#'
#' @param facet_in User input for facetting (facet_rows, facet_cols).
#' @param len Length of output vector.
#'
#' @returns Vector of facet memberships.
#'
make_facet_vector <- function(facet_in, len) {
  # Check that the indices are strictly increasing
  if (!all(facet_in == cummax(facet_in))) {
    cli::cli_abort("The facet indices must be increasing.",
                   class = "facet_ind_error")
  }

  # Check that there are no negative values
  if (any(facet_in < 0)) {
    cli::cli_abort("The facet indices cannot be negative.",
                   class = "facet_ind_error")
  }

  vec_out <- vector("numeric", len)

  # Value to use for facets (start at 1 and go up 1 for each facet)
  # If there is a zero, start facet numbers at 0
  facet_num <- ifelse(any(dplyr::near(facet_in, 0)), 0, 1)

  # Index to go from
  i_start <- 1

  # If last index is not in the facet_in, add it to go to the end
  if (!len %in% facet_in) {
    facet_in[length(facet_in) + 1] <- len
  }

  # Make the vector
  for (i_end in facet_in) {
    vec_out[seq(i_start, i_end)] <- facet_num
    facet_num <- facet_num + 1
    i_start <- i_end + 1
  }

  return(vec_out)
}
