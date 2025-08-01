% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gghm.R
\name{prepare_mixed_param}
\alias{prepare_mixed_param}
\title{Prepare parameters for mixed layouts.}
\usage{
prepare_mixed_param(param, param_name)
}
\arguments{
\item{param}{Parameter to prepare.}

\item{param_name}{Parameter name (for error messages and handling special parameter).}
}
\value{
If length one, it is returned duplicated in a list for use in each triangle. If longer than 1
an error message is returned. In other cases, the input parameter is returned (length two input).
}
\description{
Prepare parameters for mixed layouts.
}
\keyword{internal}
