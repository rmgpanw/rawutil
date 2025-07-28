# OVERVIEW ----------------------------------------------------------------

# TODO --------------------------------------------------------------------


# EXPORTED FUNCTIONS ------------------------------------------------------

#' Display time taken message
#'
#' Helper function that displays formatted time taken messages within other functions.
#' Calculates and displays the elapsed time since a start time obtained from
#' `proc.time()`.
#'
#' The message format is: **"Time taken: X minutes, Y seconds."**
#'
#' @param start_time **numeric vector**. Start time obtained from `proc.time()`.
#'
#' @return **message**. Prints a formatted time elapsed message to console.
#' @export
#' @examples
#' # Create a function that sleeps and displays elapsed time
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
#' Similar to the [pandas.DataFrame.replace](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.replace.html)
#' method using a dictionary for the `value` argument. Replaces values in a vector
#' based on a named mapping.
#'
#' **Note:** Only works with vectors of type `numeric` (including `integer`) or `character`.
#'
#' @param x **vector**. The vector to be relabelled (numeric, integer, or character).
#' @param dict **named vector**. Mapping where `names(dict)` are the existing values
#'   to be replaced and values are the replacements. Names must be unique.
#' @param default_value **scalar**. Default value for vector elements not present in
#'   `names(dict)`. If `NULL` (default), unmatched values remain unchanged.
#' @param suppress_warnings **logical**. If `FALSE` (default), warns when the vector
#'   contains values not present in `dict`.
#'
#' @return **vector**. The input vector with values replaced according to `dict`.
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
#' Similar to the [pandas.DataFrame.replace](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.replace.html)
#' method using a dictionary for the `value` argument. Replaces values in a specific
#' dataframe column based on a named mapping.
#'
#' **Note:** Only works with columns of type `numeric` (including `integer`) or `character`.
#'
#' @param df **data.frame**. The input dataframe.
#' @param colname **character**. Name of the column to be relabelled.
#' @param dict **named vector**. Mapping where `names(dict)` are the existing values
#'   in `df[[colname]]` to be replaced and values are the replacements. Names must be unique.
#' @param default_value **scalar**. Default value for column elements not present in
#'   `names(dict)`. If `NULL` (default), unmatched values remain unchanged.
#' @param suppress_warnings **logical**. If `FALSE` (default), warns when the column
#'   contains values not present in `dict`.
#'
#' @return **data.frame**. The input dataframe with specified column values replaced.
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

#' Print a data frame as a call to tibble()
#'
#' Convenience function that prints a data frame to the console formatted as a
#' call to `tibble::tibble()`. Useful for quickly converting small data frames
#' into reproducible R code.
#'
#' **Features:**
#' - Handles character/factor columns with proper quoting
#' - Converts `NA` values to proper R `NA` syntax
#' - Formats numeric columns without quotes
#'
#' @param df **data.frame**. The data frame to convert to tibble syntax.
#'
#' @return **NULL**. Prints formatted output to console.
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
               "),\n") %>%
          # for NA values in character columns, convert 'NA' to NA
          stringr::str_replace_all(pattern = "'NA'",
                                   replacement = "NA"))
  }
}

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
