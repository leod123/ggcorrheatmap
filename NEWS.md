# ggcorrheatmap 0.3.0

## New features

* The new `split_diag` argument allows for triangular cells in the diagonal.

* Square matrices where the row- and column names are the same now work with features that previously required a symmetric matrix (such as triangular layouts).

## Minor changes

* `return_data` now always returns a `layout` column.

* The legends in `ggcorrhm()` no longer assume that Pearson correlation was used when `cor_in` is `TRUE`.

## Fixes

* Annotations and mixed layouts now work with ggplot2 version 4.0.0.

## Deprecated arguments

* `show_names_x/y` and `names_x/y_side` have been deprecated in favour of `show_names_cols/rows` and `names_cols/rows_side` for consistency with other arguments.

# ggcorrheatmap 0.2.0

## New features

* The scale-modifying arguments `bins`, `limits`, `high`, `mid`, `low`, and `size_range` can now take two inputs in mixed layouts.

* The new `scale_data` argument allows for scaling rows or columns.

* The new `gghm_tidy()` and `ggcorrhm_tidy()` functions can make heatmaps from long format data using tidy input.

* The new `cor_long()` function makes it convenient to calculate correlations from long format data.

* The new `add_mixed_layout()` function can add mixed layout labels to long format data to aid in cell label creation.

* The new `split_rows` and `split_cols` arguments can be used to add gaps to the heatmap via facets.

## Breaking changes

* The new `annot_rows_names_params` and `annot_cols_names_params` arguments (that provide parameters to `ggplot2::geom_text()`) supersede `annot_rows_name_params` and `annot_cols_name_params` that don't work with heatmap splits.

## Minor fixes

* The default continuous colour scale nr 7 now works.

* The `bins` argument now only accepts positive numbers and, for non-integer values, values above 3 as other values do not work.

* The 'none' mode now works with annotation.

# ggcorrheatmap 0.1.2

* Initial CRAN release.

## Minor fixes

* `return_data` now returns the correct number of rows in mixed layouts.

