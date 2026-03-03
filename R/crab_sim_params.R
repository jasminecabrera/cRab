#' Choosing simulation parameters for cRab
#'
#' @description
#' Updates crab_prep() object allowing the user to specify simulation-specific
#' parameters to be used in crab().
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
#' @param n
#' Number of observations to generate
#'
#' Note: If n does not split evenly by desired_k, it will be rounded up
#'
#' Defaults to 100
#'
#' @param covariance
#' List of variance values or single value used to construct the covariance
#' matrix for the dataset
#'
#' @param desired_k
#' The specific number of clusters (k) to evaluate
#'
#' Defaults to 3
#'
#' @param subsample_prop
#' Proportion of the dataset to use for each resample
#'
#' Defaults to 0.8
#'
#' @param num_runs
#' Number of iterations
#'
#' @param cluster_method
#' Specified clustering algorithm
#'
#' @returns
#' An updated list containing the data, its original column names, NULL for
#' user-specified columns (since the data is simulated), and the user-defined
#' simulation parameters.
#'
#' @export
#'
#' @examples
#' # must run simulated data first
#' simulated_data <- simulate_mvn(n = 10)
#'
#' # create crab_setup with data, prep, and sim. params pipeline
#' crab_setup1 <- simulated_data |>
#'                crab_prep() |>
#'                crab_sim_params(min_k = 2, max_k = 3, num_subsamples = 10,
#'                                num_runs = 2, covariance = 0.3,
#'                                cluster_method = "kmeans")
#' crab_setup1

crab_sim_params <- function(data_info, min_k, max_k, num_subsamples,
                            n = 100, covariance, desired_k = 3,
                            subsample_prop = 0.8, num_runs, cluster_method){
  # check min_k and max_k
  if (min_k > max_k){
    stop("Error: min_k must be <= max_k.")}

  if (min_k < 0){
    stop("Error: min_k must be > 0.")}

  if (max_k < 0){
    stop("Error: max_k must be > 0.")}

  # check subsample prop
  if (subsample_prop >= 1){
    stop("Error: subsample_prop must be < 1.")}

  # check num_runs
  if (num_runs < 0){
    stop("Error: num_runs must be > 0.")}

  # update specified params
  data_info$specified_params <- list(
    min_k = min_k,
    max_k = max_k,
    num_subsamples = num_subsamples,
    n = n,
    covariance = covariance,
    desired_k = desired_k,
    subsample_prop = subsample_prop,
    num_runs = num_runs,
    cluster_method = cluster_method)

  return(data_info)}
