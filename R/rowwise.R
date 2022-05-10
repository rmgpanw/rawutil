#' Summarise a group of columns row-wise
#'
#' Helper function
#'
#' @param dt A data table.
#' @param function_name A function name as a character.
#' @param selected_cols Character vector of column names.
#' @param new_colname Name of new summary column.
#' @noRd
rowwise_summary <- function(dt,
                            function_name,
                            selected_cols,
                            new_colname) {

  ukb_main[, c(new_colname) := purrr::map(
    function_name,
    dt_rowwise_fn,
    .SD), .SDcols = selected_cols]

  return(ukb_main)
}

#' Helper function for rowwise_summary()
#'
#' @param fn a character vector of functions
#' @param cols a character vector of column names to summarise with functions in
#'   fn
#' @param ... allows additional arguments such as na.rm to passed on to
#'   functions listed in fn
#' @noRd
dt_rowwise_fn <- function(fn, cols, ...) {
  # Row-wise summary helper function:
  # applies a set of summary functions ('fn' =
  # character vector of function names) across selected columns ('cols' =
  # character vector of column names in a datatable) using apply(). '...' allows
  # additional arguments to be passed on to functions (e.g. 'na.rm = ')
  apply(cols, MARGIN = 1, fn, ...)
}
