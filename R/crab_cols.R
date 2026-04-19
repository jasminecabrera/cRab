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
  ### INPUT CHECKS:
  # check if data_info is a list
  if (!is.list(data_info)) {
    stop("Input must be a list (output from crab_prep function).")}

  # correct element names
  required_names <- c("data", "original_cols", "specified_cols", "specified_params")
  if (!all(required_names %in% names(data_info))) {
    stop("Input list is missing at least one of the following: 'data', 'original_cols', 'specified_cols', 'specified_params.' Did you provide the correct object?")}

  # check data is a dataframe
  if (!is.data.frame(data_info$data)) {
    stop("The 'data' element must be a dataframe.")}

  # check original_cols is a list
  if (!is.character(data_info$original_cols)) {
    stop("original_cols must be a character vector of column names.")}

  # check chosen_cols is a character
  if (!is.character(chosen_cols)) {
    stop("chosen_cols must be a character vector of column names.")}

  ### FUNCTION:
  # check if chosen columns exist
  missing_cols <- setdiff(chosen_cols, data_info$original_cols)

  # leave error message if columns dont exist
  if (length(missing_cols) > 0) {
    stop("Column(s) do not exist: ",
         paste(missing_cols, collapse = ", "))}

  # update specified cols
  data_info$specified_cols <- chosen_cols

  return(data_info)}
