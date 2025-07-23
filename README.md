
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggcorrheatmap

<!-- badges: start -->

[![R-CMD-check](https://github.com/leod123/ggcorrheatmap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/leod123/ggcorrheatmap/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/leod123/ggcorrheatmap/branch/main/graph/badge.svg)](https://app.codecov.io/gh/leod123/ggcorrheatmap/branch/main)
<!-- badges: end -->

ggcorrheatmap is a convenient package for generating correlation
heatmaps made with ggplot2, with support for triangular layouts,
clustering and annotation. As the output is a ggplot2 object you can
further customise the appearance using familiar ggplot2 functions.
Besides correlation heatmaps, there is also support for making general
heatmaps.

## Installation

You can install ggcorrheatmap from CRAN using:

``` r
install.packages("ggcorrheatmap")
```

Or you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("leod123/ggcorrheatmap")
```

## Example

Below is an example of how to generate a correlation heatmap with
clustered rows and columns and row annotation, using a triangular layout
that excludes redundant cells.

``` r
library(ggcorrheatmap)

set.seed(123)
# Make a correlation heatmap with a triangular layout, annotations and clustering
row_annot <- data.frame(.names = colnames(mtcars),
                        annot1 = sample(letters[1:3], ncol(mtcars), TRUE),
                        annot2 = rnorm(ncol(mtcars)))
ggcorrhm(mtcars, layout = "bottomright",
         cluster_rows = TRUE, cluster_cols = TRUE,
         show_dend_rows = FALSE, annot_rows_df = row_annot)
```

<img src="man/figures/README-example1-1.png" alt="A correlation heatmap showing the correlations between the columns of the mtcars dataset. Only the bottom triangle of the symmetric matrix is displayed, oriented as a bottom right triangle. The colour scale goes from blue at -1 to white at 0 and red at 1. The names of the columns are displayed on the diagonal. Below the heatmap is a dendrogram showing that the data has been clustered. Along the right side two columns are placed, showing one discrete and one continuous annotation (called annot1 and annot2, respectively). Three legends are found to the right of the plot, one for the main heatmap scale and one for each annotation." width="70%" style="display: block; margin: auto;" />

Or a mixed layout that displays different things in the different
triangles.

``` r
# With correlation values and p-values
ggcorrhm(mtcars, layout = c("topright", "bottomleft"),
         cell_labels = c(FALSE, TRUE), p_values = c(FALSE, TRUE))
```

<img src="man/figures/README-example2-1.png" alt="A correlation heatmap of mtcars columns. Half of the plot, above the diagonal, is a normal heatmap with coloured cells and the other half has white cells with coloured text inside. The diagonal has coloured cells with names column names written inside. The coloured text in the bottom triangle shows correlation values with asterisks added indicating p-value thresholds." width="70%" style="display: block; margin: auto;" />

It is also possible to make a normal heatmap, for a more flexible
output.

``` r
gghm(scale(mtcars), cluster_rows = TRUE, cluster_cols = TRUE)
```

<img src="man/figures/README-example3-1.png" alt="A heatmap of the mtcars data (scaled). Cells are coloured with a scale going from dark blue to light blue. Both rows and columns have been clustered, with dendrograms to the right and below the heatmap."  />

## More examples

There are many more options for customisation, covered in the different
articles of the package:

- [Making a
  heatmap](https://leod123.github.io/ggcorrheatmap/articles/heatmap.html)
- [Correlation
  heatmaps](https://leod123.github.io/ggcorrheatmap/articles/correlation.html)
- [Annotation](https://leod123.github.io/ggcorrheatmap/articles/annotation.html)
- [Clustering](https://leod123.github.io/ggcorrheatmap/articles/clustering.html)
- [Mixed
  layouts](https://leod123.github.io/ggcorrheatmap/articles/mixed.html)
- [Scales and
  legends](https://leod123.github.io/ggcorrheatmap/articles/legends.html)
