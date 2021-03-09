#' Calculate interaction p-values
#'
#' @param effect1 numeric vector
#' @param effect2 numeric vector
#' @param se1 numeric vector
#' @param se2 numeric vector
#' @param pvals_only logical. Indicate whether to return only interaction
#'   p-values, or a dataframe also including the input betas and standard
#'   errors. Default = \code{TRUE}
#'
#' @return df or numeric vector of interaction p-values (determined by
#'   \code{pvals_only} argument)
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
  pvalue <- pnorm(abs(difference / se_diff), lower.tail = F) * 2

  # results
  res <- data.frame(point = difference, se = se_diff, pval = pvalue)

  if (pvals_only) {
    return(res$pval)
  } else if (pvals_only == FALSE) {
    return(res)
  }
}
