% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_scales.R
\name{extract_scales}
\alias{extract_scales}
\title{Pick out relevant scales and format for mixed layout.}
\usage{
extract_scales(main_scales, scale_order, aes_type, layout)
}
\arguments{
\item{main_scales}{Scales for main plot as obtained from prepare_scales.}

\item{scale_order}{List with order of scales as obtained from make_legend_order.}

\item{aes_type}{Aesthetic for which to pick out scales.}

\item{layout}{Layout of heatmap.}
}
\value{
List of scales to use.
}
\description{
Pick out relevant scales and format for mixed layout.
}
\keyword{internal}
