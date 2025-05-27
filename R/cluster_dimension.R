#' Cluster data using hierarchical clustering or use provided clustering.
#'
#' @keywords internal
#'
#' @param cluster_data Either a logical indicating if data should be clustered, or a `hclust` object.
#' @param mat Matrix to cluster.
#' @param cluster_distance Distance metric for clustering.
#' @param cluster_method Clustering method for `hclust`.
#' @param dend_options List specifying `dendextend` functions to use.
#'
#' @returns List containing the dendrogram and clustering objects (or NULL if no clustering).
#'
cluster_dimension <- function(cluster_data, mat, cluster_distance, cluster_method, dend_options = NULL) {
  # Make dendrograms
  if (is.logical(cluster_data)) {

    clust <- hclust(dist(mat, method = cluster_distance), method = cluster_method)

    dendro <- as.dendrogram(clust)

    # Apply dendextend options if any are given
    if (is.list(dend_options)) {
      dendro <- apply_dendextend(dendro, dend_options)
    }

    dendro <- dendextend::as.ggdend(dendro)
    dendro$labels$label <- as.character(dendro$labels$label)

    return(list("dendro" = dendro, "clust" = clust))

  } else if (inherits(cluster_data, "hclust")) {
    clust <- cluster_data
    dendro <- as.dendrogram(clust)

    # Apply dendextend options if any
    if (is.list(dend_options)) {
      dendro <- apply_dendextend(dendro, dend_options)
    }

    dendro <- dendextend::as.ggdend(dendro)
    dendro$labels$label <- as.character(dendro$labels$label)

    return(list("dendro" = dendro, "clust" = clust))
  } else {
    return(NULL)
  }
}
