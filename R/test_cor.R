# If the layout is triangular, only calculate correlation and p-values for half of the matrix (and skip the diagonal since it is 1 anyway)
# Is faster than computing the whole matrix, but does not work with ggcorrhm which feeds the whole
# correlation matrix to gghm
test_cor <- function(x, y = NULL, full_plt = T, include_diag = F,
                     method = "pearson", use = "everything", p_adj_method = "none") {
  # Get the combinations to test
  if (is.null(y)) {
    y <- x
  }
  all_pairs <- shape_mat_long(matrix(nrow = ncol(x), ncol = ncol(y), dimnames = list(colnames(x), colnames(y))))
  all_pairs <- dplyr::bind_cols( # Bind together with cor, p, padj for each row
    dplyr::select(all_pairs, -value), dplyr::bind_rows(
      apply(all_pairs, 1, function(comb) {
        tst <- cor.test(x[, comb["row"]], y[, comb["col"]])
        data.frame(value = unname(tst$estimate), p_val = tst$p.value)
      }, simplify = T)
    )
  )

  # Adjust p-values
  all_pairs[["p_adj"]] <- p.adjust(all_pairs[["p_val"]], method = p_adj_method)

  return(all_pairs)
}
