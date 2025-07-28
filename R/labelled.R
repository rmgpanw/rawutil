#' Convert logical columns to numeric labelled 'Yes'/'No'
#'
#' Converts columns of type `logical` to numeric labelled variables where:
#' - `TRUE` → `1` with label "Yes"  
#' - `FALSE` → `0` with label "No"
#'
#' This is useful for creating labelled data suitable for statistical analysis
#' while preserving meaningful value labels.
#'
#' @param df **data.frame**. The input data frame.
#' @param selected_cols **character vector**. Column names to convert. Must all be
#'   logical type. If `NULL` (default), converts all logical columns.
#'
#' @return **data.frame**. Input dataframe with specified logical columns converted
#'   to labelled numeric.
#' @export
logical_to_labelled <- function(df, selected_cols = NULL) {

  if (is.null(selected_cols)) {
    result <- df %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::vars_select_helpers$where(is.logical),
          ~ .x %>%
            as.numeric() %>%
            haven::labelled(labels = c(Yes = 1,
                                       No = 0))
        )
      )
  } else {
    # `selected_cols` should be class character
    assertthat::assert_that(class(selected_cols == "character"),
                            msg = "Error! `selected_cols` must be a character vector")

    # check these columns are all type logical
    all_logical_colnames <- df %>%
      dplyr::select(tidyselect::vars_select_helpers$where(is.logical)) %>%
      names()

    assertthat::assert_that(all(selected_cols %in% all_logical_colnames),
                            msg = "Error! Not all columns in `selected_cols` are of class 'logical'")

    # convert selected cols to labelled numeric
    result <- df %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(selected_cols),
          ~ .x %>%
            as.numeric() %>%
            haven::labelled(labels = c(Yes = 1,
                                       No = 0))
        )
      )
  }

  # return result
  return(result)
}

#' Replace strings in all variable labels
#'
#' Updates all variable labels in a data frame by applying `stringr::str_replace_all()`
#' to each label attribute. Useful for batch updating of variable labels.
#'
#' @param df **data.frame**. The input data frame with labelled variables.
#' @param pattern **character**. Regular expression pattern to match (passed to `str_replace_all()`).
#' @param replacement **character**. Replacement string (passed to `str_replace_all()`).
#'
#' @return **data.frame**. Data frame with updated variable labels.
#' @export
#'
#' @family Labelled data utility functions
str_replace_all_var_labels <- function(df,
                                       pattern,
                                       replacement) {
  for (variable in names(df)) {
    if (!is.null(attributes(df[[variable]])$label)) {
      attributes(df[[variable]])$label <- stringr::str_replace_all(
        string = attributes(df[[variable]])$label,
        pattern = pattern,
        replacement = replacement
      )
    }
  }

  return(df)
}

#' Remove strings from all variable labels
#'
#' Updates all variable labels in a data frame by removing specified patterns using
#' `stringr::str_remove()`. By default, removes trailing parenthetical expressions
#' like "(f1234...)" commonly found in survey data.
#'
#' @param df **data.frame**. The input data frame with labelled variables.
#' @param pattern **character**. Regular expression pattern to remove. Default removes
#'   trailing whitespace and parenthetical expressions: `"\\s\\(f[:digit:]+.*\\)$"`.
#'
#' @return **data.frame**. Data frame with updated variable labels.
#' @export
#'
#' @family Labelled data utility functions
str_remove_from_var_labels <- function(df,
                                       pattern = "\\s\\(f[:digit:]+.*\\)$") {
  for (variable in names(df)) {
    if (!is.null(attributes(df[[variable]])$label)) {
      attributes(df[[variable]])$label <-
        stringr::str_remove(string = attributes(df[[variable]])$label,
                            pattern = pattern)
    }
  }

  return(df)
}

#' Capitalise first letter of all variable labels
#'
#' Updates all variable labels in a data frame by capitalising the first letter
#' of each label. Useful for standardising label formatting.
#'
#' @param df **data.frame**. The input data frame with labelled variables.
#'
#' @return **data.frame**. Data frame with variable labels having capitalised first letters.
#' @export
#'
#' @family Labelled data utility functions
capitalise_first_letter_var_labels <- function(df) {
  for (variable in names(df)) {
    if (!is.null(attributes(df[[variable]])$label)) {
      substr(attributes(df[[variable]])$label, 1, 1) <-
        toupper(substr(attributes(df[[variable]])$label, 1, 1))
    }
  }

  return(df)
}

