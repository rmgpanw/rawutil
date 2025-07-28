# OVERVIEW ----------------------------------------------------------------

# Functions for summarising tabular data.

# TODO --------------------------------------------------------------------


# EXPORTED FUNCTIONS ------------------------------------------------------

#' Enhanced data frame summary with proportions
#'
#' A tidyverse-friendly summary function extending `skimr::skim()` with additional
#' statistics for factor and logical variables. Works with `dplyr::group_by()` and
#' the pipe operator.
#'
#' **Enhanced features:**
#' - **Factor variables**: Shows percentage breakdown of all levels
#' - **Logical variables**: Reports percentage of `TRUE` values
#' - **All variable types**: Includes standard `skimr` statistics
#'
#' **Tip:** Convert character columns to factors first for more informative summaries.
#'
#' @param data **data.frame**. The data frame to summarize.
#' @param ... Additional arguments passed to `skimr::skim()`.
#' @param .data_name **character**. Optional name for the dataset in output.
#'
#' @return **tibble**. Summary statistics by variable type with enhanced factor/logical summaries.
#' @export
#' @examples
#' # Basic summary
#' my_skim(iris)
#'
#' # Grouped summary with factor conversion
#' library(magrittr)
#' mtcars %>%
#'   dplyr::mutate(
#'     dplyr::across(
#'       tidyselect::all_of(c("cyl", "vs", "am", "gear", "carb")),
#'       as.factor)
#'     ) %>%
#'   dplyr::group_by(am) %>%
#'   my_skim()
my_skim <- function(data,
                    ...,
                    .data_name = NULL) {
  custom_skim <- skimr::skim_with(
    # factor - a long anonymous function that converts a prop table to a single string
    factor = skimr::sfl(
      pct = function(x) {
        # make a prop table in %
        pct_table <- prop.table(table(x)) * 100

        # round % to 1dp
        pct_table <- round(pct_table, 1)

        # zip the table names and values together
        combined_vector <- vector(mode = 'character', length = 0L)
        for (i in 1:length(pct_table)) {
          combined_vector <-
            c(combined_vector, paste0(names(pct_table)[i], ":"))
          combined_vector <-
            c(combined_vector, paste0(as.character(pct_table)[i], "%,"))
        }

        # see result
        combined_vector

        # glue character vector into single string
        combined_vector <-
          stringr::str_c(combined_vector, collapse = " ")

        # return result
        return(combined_vector)
      }
    ),

    # logical - returns proportion = TRUE
    logical = skimr::sfl(
      pct_TRUE = function(x) {
        sum(x == TRUE, na.rm = TRUE) / length(x) * 100
      }
    )
  )

  custom_skim(data = data,
              .data_name = .data_name,
              ...)
}


# PRIVATE FUNCTIONS -------------------------------------------------------





