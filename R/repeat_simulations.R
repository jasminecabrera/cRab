#' Run cRab using simulated data
#'
#' @description
#' Runs crab() repeatedly for a specified number of iterations, defined by num_runs.
#'
#' @param crab_setup
#' a list containing the dataset, the original column names, and placeholder
#' NULL values for user-specified columns and parameters; the output from crab_prep()
#'
#' @param data
#'  the simulated dataset to be analyzed
#'
#'  Note: if not used, simulate_mvn() will be called
#'
#' @param num_runs
#' Number of iterations
#'
#' @param n
#' Number of observations to generate
#'
#' Note: If n does not split evenly by desired_k, it will be rounded up
#'
#' Defaults to 100

#' @param covariance
#' List of variance values or single value used to construct the covariance
#' matrix for the dataset
#'
#' @param desired_k
#' The specific number of clusters (k) to evaluate
#'
#' Defaults to 3
#'
#' @param min_k
#' The starting number of clusters (k) for evaluation
#'
#' @param max_k
#' The starting number of clusters (k) for evaluation
#'
#' @param subsample_prop
#' Proportion of the dataset to use for each resample
#'
#' Defaults to 0.8
#'
#' @param num_subsamples
#' Number of resamples to generate
#'
#' @param cluster_method
#' Specified clustering algorithm
#'
#' @param start_seed
#' Random seed for reproducibility
#'
#' Defaults to 123
#'
#'
#' @returns
#' A data frame where each row corresponds to a tested combination of k cluster,
#' covariance, and replication. The data frame includes the similarity score,
#' runtime, covariance, replication, and best_clust column. This column is blank
#' for all rows except the one with the lowest similarity score for that
#' combination within its own replication, where it is marked as "BEST".
#'
#' @export
#'
#' @examples
#' repeat_simulations(n = 100, num_runs = 5, covariance = c(0.1, 0.2),
#'                    min_k = 2, max_k = 4, num_subsamples = 10,
#'                    cluster_method = "kmeans")

repeat_simulations <- function(crab_setup = NULL,
                               data = NULL,
                               num_runs,
                               n = 100,
                               covariance,
                               desired_k = 3,
                               min_k,
                               max_k,
                               subsample_prop = 0.8,
                               num_subsamples,
                               cluster_method,
                               start_seed = 123) {

  # if crab_setup pipeline is used
  if (!is.null(crab_setup)){
    # assign data
    sim_data <- crab_setup$data

    # check params
    if (is.null(crab_setup$specified_params)){
      stop("Error: You have not specified your parameters. Call crab_sim_params() first.")}

    # assign params
    params <- crab_setup$specified_params
    num_runs = params$num_runs
    n = params$n
    min_k = params$min_k
    max_k = params$max_k
    subsample_prop = params$subsample_prop
    num_subsamples = params$num_subsamples
    cluster_method = params$cluster_method

    if(!is.null(params$covariance)){
      covariance = params$covariance
      desired_k = params$desired_k}}

  # initialize results list
  results_list <- list()

  # create counter
  counter <- 1

  # loop through covariances
  for (cov in covariance) {

    # simulate data
    if (is.null(data)) {
      sim_data <- simulate_mvn(n = n,
                               covariance = cov,
                               desired_k = desired_k)}

    # loop through number of runs/reps
    for (i in seq_len(num_runs)) {

      # alternate seed
      current_seed <- start_seed + i

      # call crab()
      sim_result <- crab(data = sim_data,
                         start_seed = current_seed,
                         n = n,
                         min_k = min_k,
                         max_k = max_k,
                         subsample_prop = subsample_prop,
                         num_subsamples = num_subsamples,
                         cluster_method = cluster_method)

      # add rep identifier
      sim_result$run <- i

      # add covariance identifier
      sim_result$cov <- cov

      # store result
      results_list[[counter]] <- sim_result

      # increase counter
      counter <- counter + 1}}

  # combine all results
  all_results <- do.call(rbind, results_list)
  return(all_results)}
