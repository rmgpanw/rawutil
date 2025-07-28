#' Create a dummy data frame for testing
#'
#' Returns a small data frame with mixed column types for testing and examples.
#' Contains 6 rows with various data types including missing values.
#'
#' **Column structure:**
#' - `chr`: character vector with letters a-e plus `NA`
#' - `int`: integer vector 1-5 plus `NA`  
#' - `fac`: factor version of chr column
#' - `log`: logical vector (5 `TRUE` values, 1 `FALSE`)
#' - `chr_rpt`: character with repeated values (3 "a", 3 "c")
#'
#' @return **data.frame**. A 6×5 data frame with mixed column types.
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
