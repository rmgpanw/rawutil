# OVERVIEW ----------------------------------------------------------------

# TODO --------------------------------------------------------------------


# EXPORTED FUNCTIONS ------------------------------------------------------

#' Display time taken message
#'
#' Helper function for displaying time taken messages within other functions.
#' Use \code{\link[base]{proc.time}} at start of function and supply this as the
#' `start_time` parameter to this function.
#'
#' @param start_time The start time.
#'
#' @return A message stating time taken since start time
#' @export
#' @examples
#' # a function that sleeps for a specified duration and displays a
#' # 'time taken' message when completed
#' sleep_fn <- function(duration) {
#'   start_time <- proc.time()
#'   Sys.sleep(duration)
#'   time_taken_message(start_time)
#' }
#'
#' sleep_fn(1)
time_taken_message <- function(start_time) {
  # get time taken
  time_taken <- proc.time() - start_time

  # display message
  message("Time taken: ",
          (time_taken[3] %/% 60),
          " minutes, ",
          (round(time_taken[3] %% 60)),
          " seconds.")
}

#' Revalue values in a vector
#'
#' Similar idea to
#' \href{https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.replace.html}{pandas.replace
#' method} using a dictionary for the \code{value} argument. \strong{Note:} only
#' works with vectors that are of type numeric (including integer) or character.
#'
#' @param x character. Name of column to be relabelled
#' @param dict a named vector. \code{names(dict)} are the 'keys' i.e. the
#'   existing values in \code{df[[colname]]} to be replaced. These should be
#'   unique. An error is raised if non-unique values are found in
#'   \code{names(dict)}
#' @param default_value default value to use for values in \code{df[[colname]]}
#'   that are not present in \code{names(dict)}. By default this is \code{NULL},
#'   meaning that values not present in \code{names(dict)} will remain
#'   unchanged.
#' @param suppress_warnings bool. A warning is raised if the column to be
#'   relabelled contains values not present in \code{dict}. This message is
#'   silenced if \code{suppress_warnings} is \code{TRUE}. Default value is
#'   \code{FALSE}.
#'
#' @return A relabelled vector.
#' @export
revalue_vector <-
  function(x,
           dict,
           default_value = NULL,
           suppress_warnings = FALSE) {

    # raise an error if column is not character/numeric/integer
    assertthat::assert_that(all(class(x) %in% c("numeric", "integer", "character")),
                            msg = paste("Error! Selected column must be of type numeric/integer/character. x is type:", class(x)))

    # `dict` is a named vector - check the names (keys) are unique
    if (length(unique(names(dict))) != length(dict)) {
      stop("names(dict) contains non-unique values")
    }

    # if default_value specified (i.e. default_value is not NULL), check length == 1
    if (!is.null(default_value)) {
      assertthat::are_equal(length(default_value), 1)
    }

    # warning message if dict does not include all values in x
    if (!suppress_warnings) {
      vals_missing_from_dict <-
        subset(x,!(x %in% names(dict)))
      if (!rlang::is_empty(vals_missing_from_dict)) {
        warning(
          paste0(
            "The column to be relabelled contains values that are not present in `dict`. Number of values = ",
            length(vals_missing_from_dict)
          )
        )
      }
    }

    # replace values
    if (is.null(default_value)) {
      # if old value is not in `dict`, then keep unchanged

      x <-  ifelse(
        test = (x %in% names(dict)),
        yes = dict[x],
        no = x
      )
    } else {
      # if old value is not in `dict`, then change to default_value
      x <-  ifelse(
        test = (x %in% names(dict)),
        yes = dict[x],
        no = default_value
      )
    }

    return(x)
  }


#' Revalue values in a dataframe column
#'
#' Similar idea to
#' \href{https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.replace.html}{pandas.replace
#' method} using a dictionary for the \code{value} argument. \strong{Note:} only
#' works with columns that are of type numeric (including integer) or character.
#'
#' @param df dataframe.
#' @param colname character. Name of column to be relabelled
#' @param dict a named vector. \code{names(dict)} are the 'keys' i.e. the
#'   existing values in \code{df[[colname]]} to be replaced. These should be
#'   unique. An error is raised if non-unique values are found in
#'   \code{names(dict)}
#' @param default_value default value to use for values in \code{df[[colname]]}
#'   that are not present in \code{names(dict)}. By default this is \code{NULL},
#'   meaning that values not present in \code{names(dict)} will remain
#'   unchanged.
#' @param suppress_warnings bool. A warning is raised if the column to be
#'   relabelled contains values not present in \code{dict}. This message is
#'   silenced if \code{suppress_warnings} is \code{TRUE}. Default value is
#'   \code{FALSE}.
#'
#' @return dataframe
#' @export
revalue_col <-
  function(df,
           colname,
           dict,
           default_value = NULL,
           suppress_warnings = FALSE) {

    # raise an error if column is not character/numeric/integer
    assertthat::assert_that(class(df[[colname]]) %in% c("numeric", "integer", "character"),
                            msg = paste("Error! Selected column must be of type numeric/integer/character. df[[colname]] is type:", class(df[[colname]])))

    # `dict` is a named vector - check the names (keys) are unique
    if (length(unique(names(dict))) != length(dict)) {
      stop("names(dict) contains non-unique values")
    }

    # if default_value specified (i.e. default_value is not NULL), check length == 1
    if (!is.null(default_value)) {
      assertthat::are_equal(length(default_value), 1)
    }

    # warning message if dict does not include all values in df[[colname]]
    if (!suppress_warnings) {
      vals_missing_from_dict <-
        subset(df[[colname]],!(df[[colname]] %in% names(dict)))
      if (!rlang::is_empty(vals_missing_from_dict)) {
        warning(
          paste0(
            "The column to be relabelled contains values that are not present in `dict`. Number of values = ",
            length(vals_missing_from_dict)
          )
        )
      }
    }

    # replace values
    if (is.null(default_value)) {
      # if old value is not in `dict`, then keep unchanged

      df[[colname]] <-  ifelse(
          test = (df[[colname]] %in% names(dict)),
          yes = dict[df[[colname]]],
          no = df[[colname]]
        )
    } else {
      # if old value is not in `dict`, then change to default_value
      df[[colname]] <-  ifelse(
        test = (df[[colname]] %in% names(dict)),
        yes = dict[df[[colname]]],
        no = default_value
      )
    }

    return(df)
  }

#' Print a data frame to the console for copying and pasting to a call to \code{tibble()}
#'
#' Convenience function
#'
#' @param df A data frame
#'
#' @return NULL.
#' @export
#'
#' @examples
#' print_df_as_call_to_tibble(head(iris))
print_df_as_call_to_tibble <- function(df) {
  for (col_name in names(df)) {
    # is this column a character or factor?
    is_character_or_factor <- is.character(df[[col_name]]) |
      is.factor(df[[col_name]])

    if (is_character_or_factor) {
      collapse <- "', '"
    } else {
      collapse <- ", "
    }

    # make type character
    df[[col_name]] <- as.character(df[[col_name]])

    # replace
    df[[col_name]][is.na(df[[col_name]])] <- "NA"

    cat(paste0(col_name,
               " = c(",
               paste0(
                 if (is_character_or_factor) {
                   "'"
                 },
                 stringr::str_c(df[[col_name]],
                                sep = "",
                                collapse = collapse),
                 if (is_character_or_factor) {
                   "'"
                 }
               ),
               "),\n") |>
          # for NA values in character columns, convert 'NA' to NA
          stringr::str_replace_all(pattern = "'NA'",
                                   replacement = "NA"))
  }
}

iris3 <- tibble::tibble(
  Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, 5.4),
  Sepal.Width = c(NA, 34, NA, NA, NA, NA),
  Petal.Length = c(1.4, 1.4, 1.3, 1.5, 1.4, 1.7),
  Petal.Width = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.4),
  Species = c('setosa', 'setosa', NA, 'setosa', 'setosa', 'setosa'),
)

# PRIVATE FUNCTIONS -------------------------------------------------------

#' Assert number is an integer that is greater than or equal to 1
#'
#' Helper function for \code{\link{fread_chunked}} and
#' \code{\link{process_df_chunked}}.
#'
#' @param x An integer >= 1. Raises an error if this condition is not met
#' @param arg_name character. The argument name for x. This is used to generate
#'   an informative error message.
#'
#' @seealso \code{\link{fread_chunked}}, \code{\link{process_df_chunked}}
assert_integer_ge_1 <- function(x, arg_name) {
  # custom error message
  error_message <- paste("Error!", arg_name, "must be an integer that is greater than 0")

  # assertion
  assertthat::assert_that(x >= 1,
                          rlang::is_integerish(x),
                          msg = error_message)
}
