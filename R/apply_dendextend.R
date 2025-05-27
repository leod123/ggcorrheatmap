#' Successively apply dendextend functions to a dendrogram.
#'
#' @keywords internal
#'
#' @param dendro Dendrogram object obtained from `stats::as.dendrogram`.
#' @param dend_list List specifying dendextend functions to apply. For usage see the details of `gg_corr_heatmap`.
#'
#' @returns A dendrogram object modified with dendextend functions.
#'
apply_dendextend <- function(dendro, dend_list) {
  # Go through options list and update the dendrogram successively with do.call
  # Use append() to make named list of input arguments
  for (i in seq_along(dend_list)) {
    # Make a temporary function by calling the provided function from dendextend
    dend_fun <- do.call(`::`, list("dendextend", names(dend_list)[i]))
    dendro <- do.call(dend_fun, append(list(dendro), dend_list[[i]]))
  }

  return(dendro)
}
