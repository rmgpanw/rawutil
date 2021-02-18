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
#' # a function that sleeps for a specified duration and displays a 'timet taken' message when completed
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

# PRIVATE FUNCTIONS -------------------------------------------------------
