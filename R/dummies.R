#' Make dummy data frame
#'
#' Returns a dataframe with 6 rows and columns of types character, integer and factor
#'
#' @return df
#' @export
#'
#' @examples
#' make_dummy_df()
#'
#' str(make_dummy_df())
make_dummy_df <- function() {
  data.frame(chr = c(letters[1:5], NA),
             int = c(1:5, NA),
             fac = as.factor(c(letters[1:5], NA)),
             log = c(rep(TRUE, 5), FALSE),
             chr_rpt = c(rep("a", 3), rep("c", 3)))
}
