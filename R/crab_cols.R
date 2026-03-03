#' Choosing columns for cRab
#'
#' @description
#' Updates crab_prep() object allowing the user to specify which dataset columns
#' should be used in crab().
#'
#' @param data_info
#' A list containing the dataset, the original column names, and placeholder
#' NULL values for user-specified columns and parameters; the output from crab_prep()
#'
#' @param chosen_cols
#' A list of column names from the dataset to be used in the analysis
#'
#'
#' @returns
#' An updated list containing the dataset, its original column names, the
#' user-selected columns, and placeholder NULL values for the remaining parameters.
#'
#' @export
#'
#' @examples
#' palmerpenguins::penguins |>
#' crab_prep() |>
#' crab_cols(c("flipper_length_mm", "bill_length_mm"))

crab_cols <- function(data_info, chosen_cols){
  # check if chosen columns exist
  missing_cols <- setdiff(chosen_cols, data_info$original_cols)

  # leave error message if columns dont exist
  if (length(missing_cols) > 0) {
    stop("Column(s) do not exist: ",
         paste(missing_cols, collapse = ", "))}

  # update specified cols
  data_info$specified_cols <- chosen_cols

  return(data_info)}
