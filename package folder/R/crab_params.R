#' Choosing parameters for cRab
#'
#' @description
#' Updates crab_prep() object allowing the user to specify which parameters should
#' be used in crab().
#'
#' @param data_info
#' A list containing the dataset, the original column names, and placeholder
#' NULL values for user-specified columns and parameters; the output from crab_prep()
#'
#' @param min_k
#' The starting number of clusters (k) for evaluation
#'
#' @param max_k
#' The ending number of clusters (k) for evaluation
#'
#' @param num_subsamples
#' Number of resamples to generate
#'
#' @param subsample_prop
#' Proportion of the dataset to use for each resample. Defaults to 0.8.
#'
#' @param cluster_method
#' Specified clustering algorithm (i.e "kmeans")
#' 
#' @param start_seed
#' Reproducibility seed for running crab()
#'
#' @returns
#' An updated list containing the dataset, its original column names, the
#' user-selected parameters, and placeholder NULL values for the specified columns.
#'
#' @export
#'
#' @examples
#' palmerpenguins::penguins |>
#' crab_prep() |>
#' crab_params(min_k = 2, max_k = 5, num_subsamples = 10, cluster_method = "kmeans")

crab_params <- function(data_info, min_k, max_k, num_subsamples,
                        subsample_prop = 0.8, cluster_method, start_seed = 123){
  
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
  
  # min_k is a positive int
  if (min_k %% 1 != 0 || min_k < 1) {
    stop("The minimum k value must be a positive integer.")}
  
  # min_k is 1
  if (min_k == 1) {
    warning("The minimum k value is 1 (one cluster).")}
  
  # min_k < max_k
  if (min_k >= max_k) {
    stop("The minimum k value must be less than the maximum k value.")}
  
  # max_k is a positive int
  if (max_k %% 1 != 0 || max_k < 1) {
    stop("The maximum k value must be a positive integer.")}
  
  # max_k is 1
  if (max_k == 1) {
    warning("The maximum k value is 1 (one cluster).")}
  
  # sub_sample prop is between 0 and 1
  if (subsample_prop <= 0 || subsample_prop >= 1) {
    stop("Subsample proportion needs to be between 0 and 1.")}
  
  #  num_subsamples is a positive int
  if (num_subsamples <= 0 || num_subsamples %% 1 != 0) {
    stop("The number of subsamples must be a positive integer.")}
  
  # clustering method is kmeans
  if (cluster_method != "kmeans"){
    stop("Error: Clustering method does not currently work. Please change to 'kmeans'.")}
  
  ### FUNCTION:
  # update specified params
  data_info$specified_params <- list(
    min_k = min_k,
    max_k = max_k,
    num_subsamples = num_subsamples,
    subsample_prop = subsample_prop,
    cluster_method = cluster_method,
    start_seed = start_seed)
  
  return(data_info)}
