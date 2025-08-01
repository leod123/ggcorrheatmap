% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout_hm.R
\name{remove_triangle}
\alias{remove_triangle}
\title{Remove triangle from symmetric matrix and return long format data.}
\usage{
remove_triangle(x, tri_remove = "upper", na_remove = FALSE)
}
\arguments{
\item{x}{Matrix to remove triangle from (and make long).}

\item{tri_remove}{Triangle to remove.}

\item{na_remove}{If NAs should be removed.}
}
\value{
Matrix in long format with triangle removed.
}
\description{
Remove triangle from symmetric matrix and return long format data.
}
\keyword{internal}
