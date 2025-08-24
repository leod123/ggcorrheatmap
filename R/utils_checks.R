#' Check that layout and mode are correct
#'
#' @keywords internal
#'
#' @param layout Plot layout.
#' @param mode Plot mode.
#'
#' @returns Error if incorrect layout or mode, otherwise nothing.
#'
check_layout <- function(layout, mode) {
  # Check layout and mode
  supported_layouts <- c("full", "f", "whole", "w",
                         "bottomleft", "bl", "topleft", "tl",
                         "topright", "tr", "bottomright", "br")
  supported_modes <- c("heatmap", "hm", "text", as.character(1:25), "none")
  mode_cli_vec <- cli::cli_vec(supported_modes, list("vec-trunc" = 6)) # For error messages

  if (!(length(mode) == 1 & length(layout) == 1) & !(length(mode) == 2 & length(layout) == 2)) {
    cli::cli_abort("{.var layout} and {.var mode} must be the same length (1 or 2).", class = "layout_mode_len_error")
  }

  if (length(layout) == 1) {
    if (!layout %in% supported_layouts) {
      cli::cli_abort(c("{.val {layout}} is not a supported layout.",
                       "i" = "Supported layouts are {.val {supported_layouts}}"),
                     class = "nonsup_layout_error")
    }

    if (!mode %in% supported_modes) {
      cli::cli_abort(c("{.val {mode}} is not a supported mode for this layout.",
                       "i" = paste0("When {.var layout} has length 1, {.var mode} must be ", cli::style_bold("one"), " of {.val {mode_cli_vec}}")),
                     class = "nonsup_mode_error")
    }

  } else if (length(layout) == 2) {
    # Only allow for combinations of topleft + bottomright and topright + bottomleft
    if (!((sum(layout %in% c("topleft", "tl")) == 1 & sum(layout %in% c("bottomright", "br")) == 1) |
          (sum(layout %in% c("topright", "tr")) == 1 & sum(layout %in% c("bottomleft", "bl")) == 1))) {
      cli::cli_abort(c("{.val {layout}} is not a supported combination of layouts.",
                       "i" = "Mixed layouts must consist of combinations of opposite triangles."),
                     class = "nonsup_layout_error")
    }
    # mode must also be length 2
    if (any(!mode %in% supported_modes)) {
      cli::cli_abort(c("{.val {mode}} is not a supported combination of modes.",
                       "i" = paste0("When {.var layout} has length 2, {.var mode} must be ", cli::style_bold("two"), " of {.val {mode_cli_vec}}")),
                     class = "nonsup_mode_error")
    }

  }
}


#' Check (supposed) logical values.
#'
#' @keywords internal
#'
#' @param ... Should be a single named argument, where the name is the variable name displayed in the error message.
#' The value is the (supposed) logical.
#' @param list_allowed Logical indicating if the argument is allowed to be a list. If TRUE each element will be checked.
#' @param call Call to use for the call in the error message (used in rlang::abort).
#' Default is rlang::caller_env() resulting in the function that called check_logical().
#'
#' @returns Error if not logical or longer than 1, otherwise nothing.
#'
check_logical <- function(..., list_allowed = FALSE, call = NULL) {
  arg <- list(...)
  name <- names(arg)
  val <- arg[[1]]

  # Get the call to use for the first part of the error
  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  # First part of error message
  err_msg <- paste0(ifelse(list_allowed, "Each element of ", ""),
                    "{.var ", name, "} must be a single {.cls logical} value, not ")

  if (isFALSE(list_allowed)) {
    # Wrong class
    if (!is.logical(val)) {
      cli::cli_abort(paste0(
        err_msg, "{.cls {class(val)}}."
      ), class = "logical_error", call = call)
    }

    # Too long
    if (length(val) > 1) {
      cli::cli_abort(paste0(
        err_msg, "{length(val)} values."
      ), class = "logical_error", call = call)
    }
  } else {

    # Per element
    sapply(val, function(v) {
      if (!is.logical(v)) {
        cli::cli_abort(paste0(
          err_msg, "{.cls {class(v)}}."
        ), class = "logical_error", call = call)
      }

      if (length(v) > 1) {
        cli::cli_abort(paste0(
          err_msg, "{length(v)} values."
        ), class = "logical_error", call = call)
      }
    })
  }
}


#' Check input numeric arguments for class and length.
#'
#' @keywords internal
#'
#' @inheritParams check_logical
#' @param allow_null Logical indicating if NULL is allowed as input for the argument.
#' @param allowed_lengths The allowed lengths of the argument.
#'
#' @returns Error if not numeric, NULL when not allowed, or too long/too short.
#'
check_numeric <- function(..., allow_null = FALSE, allowed_lengths = 1,
                          list_allowed = FALSE, call = NULL) {
  arg <- list(...)
  name <- names(arg)
  val <- arg[[1]]

  if (isFALSE(list_allowed)) {
    if (isTRUE(allow_null) && is.null(val)) {
      return(NULL)
    }
  }

  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  # Error message, taking into consideration if NULL is allowed, multiple allowed
  # lengths, and min/max allowed lengths
  err_msg <- paste0(ifelse(list_allowed, "Each element of ", ""),
                    "{.var ", name, "} must be ",
                    ifelse(length(allowed_lengths) > 1,
                           paste0(min(allowed_lengths), " to ", max(allowed_lengths)),
                           ifelse(max(allowed_lengths) > 1,
                                  max(allowed_lengths),
                                  "a single")),
                    " {.cls numeric} value", ifelse(max(allowed_lengths) > 1, "s", ""),
                    ifelse(allow_null, " or NULL", ""),
                    ", not ")

  if (isFALSE(list_allowed)) {
    # NULL but NULL not allowed
    if (isFALSE(allow_null) && is.null(val)) {
      cli::cli_abort(paste0(
        err_msg, " NULL."
      ), class = "numeric_error")
    }

    # Wrong class
    if (!is.numeric(val)) {
      cli::cli_abort(paste0(
        err_msg, " {.cls {class(val)}}."
      ), class = "numeric_error")
    }

    # Too long or too short
    if (!(length(val) <= max(allowed_lengths) &&
          length(val) >= min(allowed_lengths))) {
      cli::cli_abort(paste0(
        err_msg, " {length(val)} value", ifelse(length(val) > 1, "s", ""), "."
      ), class = "numeric_error")
    }
  } else {

    # Per element
    sapply(val, function(v) {
      if (isTRUE(allow_null) && is.null(v)) {
        return(NULL)
      }

      if (isFALSE(allow_null) && is.null(v)) {
        cli::cli_abort(paste0(
          err_msg, " NULL."
        ), class = "numeric_error")
      }

      if (!is.numeric(v)) {
        cli::cli_abort(paste0(
          err_msg, " {.cls {class(v)}}."
        ), class = "numeric_error")
      }

      if (!(length(v) <= max(allowed_lengths) &&
            length(v) >= min(allowed_lengths))) {
        cli::cli_abort(paste0(
          err_msg, " {length(v)} value", ifelse(length(v) > 1, "s", ""), "."
        ), class = "numeric_error")
      }
    })
  }

}


#' Check cell labels if they are ok.
#'
#' @keywords internal
#'
#' @param cell_labels The `cell_labels` input to `gghm()`.
#' @param x_long Long format input data.
#'
#' @returns Long format data frame containing cell labels.
#'
check_cell_labels <- function(cell_labels, x_long) {
  value <- label <- NULL

  cell_data <- if (is.matrix(cell_labels) | is.data.frame(cell_labels)) {
    # Perform the same checks as for plotting data
    # Check that there are colnames
    if (is.null(colnames(cell_labels))) {colnames(cell_labels) <- 1:ncol(cell_labels)}

    if (".names" %in% colnames(cell_labels)) {
      rownames(cell_labels) <- cell_labels[[".names"]]
      cell_labels <- dplyr::select(cell_labels, -".names")
    }
    # Explicitly define the rownames to prevent ggplot2 error if x is a data frame without explicit rownames
    rownames(cell_labels) <- rownames(cell_labels)

    # Check that there are rownames
    if (is.null(rownames(cell_labels))) {rownames(cell_labels) <- 1:nrow(cell_labels)}

    cell_labels <- as.matrix(cell_labels)

    # If input is a matrix, convert to long format
    cell_data_temp <- dplyr::rename(layout_hm(as.matrix(cell_labels)), "label" = "value")

    # Merge with input long matrix to throw away irrelevant rows
    dplyr::select(dplyr::left_join(x_long, cell_data_temp, by = c("row", "col")), "row", "col", "label")

  } else if (isTRUE(cell_labels)) {
    # If just a TRUE, use values as cell labels
    dplyr::select(dplyr::mutate(x_long, label = value), "row", "col", "label")

  } else if (isFALSE(cell_labels)) {
    return(cell_labels)

  } else {
    cli::cli_abort("{.var cell_labels} should be TRUE to write the cell values, or
                  a {.cls matrix} or {.cls data.frame} that shares row/colnames with the plotted matrix.",
                   class = "cell_labels_class_error")
  }

  # Skip NA labels
  cell_data <- subset(cell_data, !is.na(label))

  if (nrow(cell_data) < 1) {
    cli::cli_warn("There are no cells in {.var cell_labels} that correspond to cells in the plotted data.",
                  class = "cell_labels_rowcol_warn")
  }

  return(cell_data)
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


#' Check annotation name parameters for deprecated usage.
#'
#' @keywords internal
#'
#' @param new_params New argument input.
#' @param old_params Deprecated argument input.
#' @param context Context (rows or cols).
#'
#' @returns A string stating which strategy to use for drawing names.
#'
check_annot_names_deprecated <- function(new_params = NULL, old_params = NULL, context = c("rows", "cols")) {
  # Make warning message
  if (!is.null(old_params)) {
    old_name <- paste0('annot_', context[1], '_name_params')
    new_name <- paste0('annot_', context[1], '_names_params')
    msg <- "The {.var {old_name}} argument is deprecated from ggcorrheatmap version 0.2.0 and does not work with heatmap gaps.
            Please use {.var {new_name}} which takes {.var ggplot2::geom_text()} parameters instead."
  }

  if (!is.null(new_params) && !is.null(old_params)) {
    # If both supplied use new
    cli::cli_warn(c(msg, i = "Both were supplied, using {.var {new_name}}."),
                  class = "annot_names_depr_both")
    params_out <- "geom"

  } else if (is.null(new_params) && !is.null(old_params)) {
    # If only old is supplied, use that
    cli::cli_warn(msg, class = "annot_names_depr")
    params_out <- "grid"

  } else if (!is.null(new_params) && is.null(old_params)) {
    params_out <- "geom"

  } else {
    params_out <- "geom"
  }

  return(params_out)
}
