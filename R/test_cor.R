#' Calculate correlations and p-values between columns.
#'
#' @keywords internal
#'
#' @param x Matrix or data frame with columns to correlate.
#' @param y Matrix or data frame, if provided will be correlated with x.
#' @param method Passed to `stats::cor`.
#' @param use Passed to `stats::cor`.
#' @param p_adj_method P-value adjustment method, passed to `stats::p.adjust`.
#'
#' @returns Data frame in long format with correlation values and nominal and adjusted p-values.
#'
test_cor <- function(x, y = NULL, method = "pearson", use = "everything", p_adj_method = "none") {
  value <- rowcol <- p_val <- p_adj <- NULL

  if (is.null(y) | identical(x, y)) {
    test_pairs <- shape_mat_long(matrix(nrow = ncol(x), ncol = ncol(x), dimnames = list(colnames(x), colnames(x))), unique_pairs = TRUE)

    # Make a data frame of all relevant combinations to fill in with values later, to avoid running tests for duplicated pairs or diagonals
    all_pairs <- shape_mat_long(matrix(nrow = ncol(x), ncol = ncol(x), dimnames = list(colnames(x), colnames(x))))
    all_pairs <- dplyr::select(all_pairs, -value)

    # Get correlations and p-values only for half of the non-diagonal pairs
    test_pairs <- dplyr::filter(test_pairs, as.character(row) != as.character(col))
    test_pairs <- dplyr::bind_cols( # Bind together with cor, p, padj for each row
      dplyr::select(test_pairs, -value), dplyr::bind_rows(
        apply(test_pairs, 1, function(comb) {
          tst <- cor.test(x[, comb["row"]], x[, comb["col"]], method = method, use = use)
          data.frame(value = unname(tst[["estimate"]]), p_val = tst[["p.value"]])
        }, simplify = TRUE)
      )
    )

    # Adjust p-values
    test_pairs[["p_adj"]] <- tryCatch(p.adjust(test_pairs[["p_val"]], method = p_adj_method),
                                      error = function(err) {
                                        cli::cli_abort(c("Error in p.adjust when adjusting p-values:",
                                                         i = err[["message"]]),
                                                       call = call("test_cor"),
                                                       class = "p_adjust_error")
                                      })

    # Fill the rest of the values
    # Make column with ID of combination, join by ID to fill in other half of the matrix
    test_pairs[["rowcol"]] <- apply(test_pairs, 1, function(i) paste(sort(c(i["row"], i["col"])), collapse = "_"))
    all_pairs[["rowcol"]] <- apply(all_pairs, 1, function(i) paste(sort(c(i["row"], i["col"])), collapse = "_"))
    all_pairs <- dplyr::left_join(all_pairs, dplyr::select(test_pairs, value, rowcol, p_val, p_adj), by = "rowcol")
    all_pairs <- dplyr::select(all_pairs, -rowcol)

    # Fill in diagonal values
    all_pairs <- dplyr::mutate(all_pairs,
                               value = dplyr::case_when(row == col ~ 1, TRUE ~ value),
                               p_val = dplyr::case_when(row == col ~ 0, TRUE ~ p_val),
                               p_adj = dplyr::case_when(row == col ~ 0, TRUE ~ p_adj))

    return(all_pairs)

  } else {
    all_pairs <- shape_mat_long(matrix(nrow = ncol(x), ncol = ncol(y), dimnames = list(colnames(x), colnames(y))))
    all_pairs <- dplyr::bind_cols( # Bind together with cor, p, padj for each row
      dplyr::select(all_pairs, -value), dplyr::bind_rows(
        apply(all_pairs, 1, function(comb) {
          tst <- cor.test(x[, comb["row"]], y[, comb["col"]], method = method, use = use)
          data.frame(value = unname(tst[["estimate"]]), p_val = tst[["p.value"]])
        }, simplify = TRUE)
      )
    )

    # Adjust p-values
    all_pairs[["p_adj"]] <- tryCatch(p.adjust(all_pairs[["p_val"]], method = p_adj_method),
                                     error = function(err) {
                                       cli::cli_abort(c("Error in p.adjust when adjusting p-values:",
                                                        i = err[["message"]]),
                                                      call = call("test_cor"),
                                                      class = "p_adjust_error")
                                     })

    return(all_pairs)
  }
}
