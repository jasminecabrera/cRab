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
#' Proportion of the dataset to use for each resample
#'
#' Defaults to 0.8
#'
#' @param cluster_method
#' Specified clustering algorithm
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
                        subsample_prop = 0.8, cluster_method){
  # check min_k and max_k
  if (min_k > max_k){
    stop("Error: min_k must be <= max_k.")}

  # check subsample prop
  if (subsample_prop >= 1){
    stop("Error: subsample_prop must be < 1.")}

  # update specified params
  data_info$specified_params <- list(
    min_k = min_k,
    max_k = max_k,
    num_subsamples = num_subsamples,
    subsample_prop = subsample_prop,
    cluster_method = cluster_method)

  return(data_info)}
