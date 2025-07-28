#' Calculate interaction p-values
#'
#' Computes p-values for testing the difference between two effect estimates.
#' Uses a two-tailed Z-test based on the difference between effects and their
#' combined standard error.
#'
#' **Formula:** 
#' - Difference = `effect1 - effect2`
#' - SE_difference = √(SE₁² + SE₂²)  
#' - Z-score = |Difference| / SE_difference
#' - P-value = 2 × P(Z > |z-score|)
#'
#' @param effect1 **numeric vector**. First set of effect estimates (e.g., beta coefficients).
#' @param effect2 **numeric vector**. Second set of effect estimates for comparison.
#' @param se1 **numeric vector**. Standard errors corresponding to `effect1`.
#' @param se2 **numeric vector**. Standard errors corresponding to `effect2`.
#' @param pvals_only **logical**. If `TRUE` (default), returns only p-values.
#'   If `FALSE`, returns a data frame with differences, standard errors, and p-values.
#'
#' @return If `pvals_only = TRUE`, returns a **numeric vector** of p-values.
#'   If `pvals_only = FALSE`, returns a **data.frame** with columns:
#'   - `point`: effect differences
#'   - `se`: standard errors of differences  
#'   - `pval`: two-tailed p-values
#' @export
calc_interaction_pvalues <- function(effect1,
                                     effect2,
                                     se1,
                                     se2,
                                     pvals_only = TRUE) {
  # difference
  difference <- effect1 - effect2

  # SE of the difference
  se_diff <- sqrt(se1^2 + se2^2)

  # test
  pvalue <- stats::pnorm(abs(difference / se_diff), lower.tail = F) * 2

  # results
  res <- data.frame(point = difference, se = se_diff, pval = pvalue)

  if (pvals_only) {
    return(res$pval)
  } else if (pvals_only == FALSE) {
    return(res)
  }
}
