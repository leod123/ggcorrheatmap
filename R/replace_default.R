#' Replace default elements in a named list with corresponding elements in a new list.
#'
#' @keywords internal
#'
#' @param default_param Named list with elements to potentially replace ("defaults").
#' @param new_param Named list with elements to replace with.
#' @param add_new Logical, if TRUE elements with names unique to `new_param` will be added to the output.
#' @param warning_context String to add to the beginning of warning message if any unsupported parameters are detected (if add_new is FALSE).
#' Default (NULL) produces no warning.
#'
#' @returns A named list where overlapping elements with overlapping names are replaced.
#'
replace_default <- function(default_param, new_param, add_new = FALSE, warning_context = NULL) {
  UseMethod("replace_default")
}

#' @export
replace_default.list <- function(default_param, new_param, add_new = FALSE, warning_context = NULL) {

  # Ignore unnamed elements
  new_param <- new_param[names(new_param) != ""]

  # Overlapping names
  overlap_names <- intersect(names(default_param), names(new_param))

  # Overwrite default values whose names exist in the specified parameters
  default_param[overlap_names] <- new_param[overlap_names]

  # Add values from elements that are unique to the new list
  if (add_new) {
    new_names <- setdiff(names(new_param), names(default_param))
    default_param[new_names] <- new_param[new_names]
  } else if (!is.null(warning_context)) {
    # If new names are not allowed, warn about those that were dropped
    if (any(!names(new_param) %in% overlap_names)) {
      dropped_names <- names(new_param)[which(!names(new_param) %in% overlap_names)]
      cli::cli_warn(c(paste0(warning_context, "The parameter{?s} {.val {dropped_names}} {?was/were} not recognised and {?was/were} dropped."),
                      i = "Supported parameter{?s} {?is/are} {.var {names(default_param)}}."),
                    class = "replace_default_warn")
    }
  }

  return(default_param)
}

#' @export
replace_default.gpar <- function(default_param, new_param, add_new = FALSE, warning_context = NULL) {
  # Special treatment for gpar objects as their indexing works differently
  # Convert to lists
  new_param <- lapply(new_param, function(x) x)
  default_param <- lapply(default_param, function(x) x)

  # Pass the lists to replace_default
  default_param <- replace_default(default_param, new_param, add_new = TRUE)

  # Convert back to gpar class
  default_param <- do.call(grid::gpar, default_param)

  return(default_param)
}
