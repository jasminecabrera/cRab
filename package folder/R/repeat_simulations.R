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
  # setup object overwrites arguments if provided
  if (!is.null(crab_setup)){
    if (!is.list(crab_setup) || is.null(crab_setup$specified_params)) {
      stop("crab_setup must be created using the pipeline: crab_prep() |> crab_sim_params().")}
    
    # unpack params
    params         <- crab_setup$specified_params
    num_runs       <- params$num_runs
    n              <- params$n
    min_k          <- params$min_k
    max_k          <- params$max_k
    subsample_prop <- params$subsample_prop
    num_subsamples <- params$num_subsamples
    cluster_method <- params$cluster_method
    variance       <- params$variance
    desired_k      <- params$desired_k
    
    # will be null
    input_data     <- crab_setup$data} 
  
  # no setup (also will be null)
  else {input_data <- NULL}
  
  # check variance
  if (any(variance <= 0)){
    stop("All variance values must be greater than 0.")}
  
  # check min_k
  if (min_k %% 1 != 0 || min_k < 1) {
    stop("min_k must be a positive integer.")}
  
  # check max_k
  if (max_k %% 1 != 0 || max_k < 1) {
    stop("max_k must be a positive integer.")}
  
  # check min_k < max_k
  if (min_k >= max_k) {
    stop("min_k must be less than max_k.")}
  
  # min_k = 1
  if (min_k == 1 || max_k == 1) {
    warning("At least one of the min_k and max_k values provided is 1 (one cluster).")}
  
  # num_runs
  if (num_runs %% 1 != 0 || num_runs < 1) {
    stop("num_runs must be a positive integer.")}
  
  # subsample_prop
  if (subsample_prop <= 0 || subsample_prop >= 1) {
    stop("subsample_prop must be between 0 and 1.")}
  
  # nun_subsamples
  if (num_subsamples %% 1 != 0 || num_subsamples <= 0) {
    stop("num_subsamples must be a positive integer.")}
  
  if (num_subsamples < 5) {
    warning("A low number of subsamples may lead to unstable CRAB scores.")}
  
  ### FUNCTION:
  # initialize results list
  results_list <- list()
  counter <- 1
  
  # loop through variances
  for (var in variance) {
    
    # simulate data
    sim_data <- simulate_mvn(n = n,
                             variance = var,
                             desired_k = desired_k)
    
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