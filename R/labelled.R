#' Convert columns of type logical to numeric labelled 'yes'/'no'
#'
#' Values in logical columns are converted from \code{TRUE}/\code{FALSE} to
#' \code{1}/\code{2}, with labels 'Yes'/'No'.
#'
#' @param df data frame
#' @param selected_cols character. A vector of selected column names to convert
#'   to labelled numeric. These should all be of type logical.
#'
#' @return A data frame
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
