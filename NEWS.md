# ggcorrheatmap (development version)

## New features

* The scale-modifying arguments `bins`, `limits`, `high`, `mid`, `low`, and `size_range` can now take two inputs in mixed layouts.

* The new `gghm_tidy()` and `ggcorrhm_tidy()` functions can make heatmaps from long format data using tidy input.

* The new `cor_long()` function makes it convenient to calculate correlations from long format data.

* The new `add_mixed_layout()` function can add mixed layout labels to long format data to aid in cell label creation.

* The new `facet_rows` and `facet_cols` arguments can be used to add gaps to the heatmap via facetting.

## Breaking changes

* The `annot_rows_name_params` and `annot_cols_name_params` arguments now take lists of static aesthetics for `ggplot2::geom_text()` instead of `ggplot2::annotation_custom()`.

## Minor fixes

* The default continuous colour scale nr 7 now works.

* The `bins` argument will no longer take non-integer values below 3 as they don't work. Negative numbers are also not accepted (integer or not).

# ggcorrheatmap 0.1.2

* Initial CRAN release.

## Minor fixes

* `return_data` now returns the correct number of rows in mixed layouts.

