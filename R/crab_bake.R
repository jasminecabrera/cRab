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
  # check cols are chosen
  if (is.null(data_info$specified_cols)){
    stop("No columns have been specified. Call crab_cols() first.")}

  # check params are chosen
  if (is.null(data_info$specified_params)){
    stop("No parameters have been specified. Call crab_params() first.")}

  # grab data with chosen cols
  crab_data <- data_info$data[, data_info$specified_cols, drop = FALSE]

  # grab params
  params <- data_info$specified_params

  if(!is.null(params$covariance)){
    # call crabs
    score <- crab(data = crab_data,
                  n = params$n,
                  covariance = params$covariance,
                  desired_k = params$desired_k,
                  min_k = params$min_k,
                  max_k = params$max_k,
                  subsample_prop = params$subsample_prop,
                  num_subsamples = params$num_subsamples,
                  cluster_method = params$cluster_method)}
  else{
    score <- crab(data = crab_data,
                  min_k = params$min_k,
                  max_k = params$max_k,
                  subsample_prop = params$subsample_prop,
                  num_subsamples = params$num_subsamples,
                  cluster_method = params$cluster_method)}

  return(score)}
