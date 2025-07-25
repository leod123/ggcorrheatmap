% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_dendrogram.R
\name{orient_dendrogram}
\alias{orient_dendrogram}
\title{Orient a dendrogram.}
\usage{
orient_dendrogram(dend, dim = c("rows", "cols"), full_plt, layout, dend_side)
}
\arguments{
\item{dend}{Dendrogram segments or nodes data frame (containing x, y, xend, yend).}

\item{dim}{String, rows or cols to know which dimensions dendrogram should be on.}

\item{full_plt}{Logical indicating if it's for the full layout.}

\item{layout}{The heatmap layout to take mixed layout into account.}

\item{dend_side}{Logical indicating if the dendrogram should be placed on the left (if row dend) or bottom (if col dend).}
}
\value{
The input dendrogram data frame but rotated and mirrored to fit the plot.
}
\description{
Orient a dendrogram.
}
\keyword{internal}
