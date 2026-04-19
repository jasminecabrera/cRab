#' Calling cRab function with crab_bake()
#'
#' @description
#' Retrieves the user-specified columns and parameters from the crab_prep()
#' object and uses them to execute crab().
#'
#' @param data_info
#' A list containing the dataset, the original column names, and placeholder NULL
#' values for user-specified columns and parameters; the output from crab_prep()
#'
#' @returns
#' A data frame where each row corresponds to a tested k cluster. The data frame
#' includes the similarity score, runtime, and a best_clust column, which is blank
#' for all rows except the one with the lowest similarity score, where it is
#' marked as "BEST".
#'
#' @export
#'
#' @examples
#' palmerpenguins::penguins |>
#' crab_prep() |>
#' crab_cols(c("flipper_length_mm", "bill_length_mm")) |>
#' crab_params(min_k = 2, max_k = 5, num_subsamples = 30, cluster_method = "kmeans") |>
#' crab_bake()

crab_bake <- function(data_info){

  ### INPUT CHECKS:
  # check if data_info is a list
  if (!is.list(data_info)) {
    stop("Input must be a list (output from crab_prep function).")}

  # correct element names
  required_names <- c("data", "original_cols", "specified_cols", "specified_params")
  if (!all(required_names %in% names(data_info))) {
    stop("Input list is missing at least one of the following: 'data', 'original_cols',
         'specified_cols', 'specified_params.' Did you provide the correct object?")}

  # check data is a dataframe
  if (!is.data.frame(data_info$data)) {
    stop("The 'data' element must be a dataframe.")}

  # check original_cols is a list
  if (!is.character(data_info$original_cols)) {
    stop("original_cols must be a character vector of column names.")}

  # check specified_params is a list
  if (!is.list(data_info$specified_params)) {
    stop("The 'specified_params' element must be a list.")}

  # check cols are chosen
  if (is.null(data_info$specified_cols)){
    stop("No columns have been specified. Call crab_cols() first.")}

  # check params are chosen
  if (is.null(data_info$specified_params)){
    stop("No parameters have been specified. Call crab_params() first.")}

  ### FUNCTION:
  # grab data with chosen cols
  crab_data <- data_info$data[, data_info$specified_cols, drop = FALSE]

  # grab params
  params <- data_info$specified_params

  # call crab()
  score <- crab(data = crab_data,
                  min_k = params$min_k,
                  max_k = params$max_k,
                  subsample_prop = params$subsample_prop,
                  num_subsamples = params$num_subsamples,
                  cluster_method = params$cluster_method)

  return(score)}
