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
#' The simulated dataset to be analyzed. Note: if not used, simulate_mvn() will
#' be called.
#'
#' @param num_runs
#' Number of iterations
#'
#' @param n
#' Number of observations to generate. Defaults to 100. Note: If n does not split
#' evenly by desired_k, it will be rounded up.
#'
#' @param covariance
#' List of variance values or single value used to construct the covariance
#' matrix for the dataset
#'
#' @param desired_k
#' The specific number of clusters (k) to evaluate. Defaults to 3.
#'
#' @param min_k
#' The starting number of clusters (k) for evaluation
#'
#' @param max_k
#' The starting number of clusters (k) for evaluation
#'
#' @param subsample_prop
#' Proportion of the dataset to use for each resample. Defaults to 0.8.
#'
#' @param num_subsamples
#' Number of resamples to generate
#'
#' @param cluster_method
#' Specified clustering algorithm
#'
#' @param start_seed
#' Random seed for reproducibility. Defaults to 123.
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
#' repeat_simulations(n = 100, num_runs = 5, variance = c(0.1, 0.2),
#'                    min_k = 2, max_k = 4, num_subsamples = 10,
#'                    cluster_method = "kmeans")

repeat_simulations <- function(crab_setup = NULL,
                               data = NULL,
                               num_runs,
                               n = 100,
                               variance,
                               desired_k = 3,
                               min_k,
                               max_k,
                               subsample_prop = 0.8,
                               num_subsamples,
                               start_seed = 123,
                               cluster_method) {

  ### INPUT CHECKS:
  # data is a dataframe
  if (!is.null(data) && !is.data.frame(data)) {
    stop("Data must be in a dataframe.")}

  # both crab_setup or data is used
  if (!is.null(crab_setup) && !is.null(data)) {
    stop("Error: Specify either crab_setup OR data, not both.")}

  # either crab_setup or data is used
  if (is.null(crab_setup) && is.null(data) && missing(variance)) {
    stop("Error: You must provide a setup object, a dataset, or a variance to simulate data.")}



  # n is an int
  if (n %% 1 != 0 || n <= 0) {
    stop("The number of observations, n, must be a positive integer.")}

  # either variance (for simulate_mvn) or given data is used
  if (!is.null(crab_setup) && !missing(variance)) {
    stop("Error: Specify either crab_setup OR variance, not both.")}

  # missing variance and crab_setup
  if (is.null(crab_setup) && missing(variance)) {
    stop("Error: You must specify either crab_setup or variance.")}

  # desired_k is an positive int
  if (desired_k %% 1 != 0 || desired_k < 1) {
    stop("The desired k value must be a positive integer.")}

  # desired_k is 1
  if (desired_k == 1) {
    warning("The desired k value is 1 (one cluster).")}



  # sub_sample prop is between 0 and 1
  if (subsample_prop <= 0 || subsample_prop >= 1) {
    stop("Subsample proportion needs to be between 0 and 1.")}



  # start seed is a positive int
  if (start_seed %% 1 != 0 || start_seed < 0) {
    stop("The seed value must be an integer.")}


  ### FUNCTION:
  # if crab_setup provided, use params
  if (!is.null(crab_setup)){

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
    variance = params$variance
    desired_k = params$desired_k

    # if no data provided
    if (is.null(data)){
      data <- crab_setup$data}}

  # num_runs is a positive int
  if (num_runs %% 1 != 0 || num_runs < 1) {
    stop("The number of runs value must be a positive integer.")}

  # num_runs is 1
  if (num_runs == 1) {
    warning("The number of runs value is 1.")}

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

  #  num_subsamples is a positive int
  if (num_subsamples <= 0 || num_subsamples %% 1 != 0) {
    stop("The number of subsamples must be a positive integer.")}

  # clustering method is kmeans
  if (cluster_method != "kmeans"){
    stop("Error: Clustering method does not currently work. Please change to 'kmeans'.")}

  # initialize results list
  results_list <- list()
  counter <- 1

  # loop through variances
  for (var in variance) {

    # variance is greater than 0
    if (var <= 0) {
      stop("Variance must be greater than 0.")}

    # simulate data if crab_setup was not provided
    if (is.null(data)) {
      sim_data <- simulate_mvn(n = n,
                               variance = var,
                               desired_k = desired_k)}
    else {sim_data <- data}

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

      # add rep and variance identifier
      sim_result$run <- i
      sim_result$var <- var

      # store result
      results_list[[counter]] <- sim_result
      counter <- counter + 1}}

  # combine all results and get rid of index row names
  all_results <- do.call(rbind, results_list)
  rownames(all_results) <- NULL

  return(all_results)}
