#' Replace default elements in a named list with corresponding elements in a new list.
#'
#' @param default_param Named list with elements to potentially replace ("defaults").
#' @param new_param Named list with elements to replace with.
#'
#' @returns A named list where overlapping elements with overlapping names are replaced.
#' @export
#'
#' @examples
#' replace_default(list(a = 1, b = 2, c = 3), list(a = 123, 456, d = 789))
replace_default <- function(default_param, new_param, add_new = F) {

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
  }

  return(default_param)
}
