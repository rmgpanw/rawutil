# OVERVIEW ----------------------------------------------------------------

# Functions for summarising tabular data.

# TODO --------------------------------------------------------------------


# EXPORTED FUNCTIONS ------------------------------------------------------

#' Summarise a dataframe
#'
#' A tidyverse-friendly summary function that summarises a dataframe by column
#' type.
#'
#' Works with \code{dplyr::group_by()} and the pipe. See the
#' \code{\link[skimr]{skim}} documentation for more details. Adapts the
#' \code{skimr::skim()} function to include proportion counts for factor
#' variables
#'
#' In general, more informative results are returned if character-type columns
#' are first converted to factors (see examples below)
#'
#' @inheritParams skimr::skim
#' @examples
#' # summarise the iris dataset
#' my_skim(iris)
#'
#' # summarise the mtcars dataset by transmissions type ("am": 0 = automatic, 1 = manual)
#' library(magrittr)
#' mtcars %>%
#'   dplyr::mutate(
#'     dplyr::across(
#'       tidyselect::all_of(c("cyl", "vs", "am", "gear", "carb")),
#'       as.factor)
#'     ) %>%
#'   dplyr::group_by(am) %>%
#'   my_skim()
#'
#' @export
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





