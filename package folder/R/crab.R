#' Calling cRab
#'
#' @description
#' Computes cluster score for clusters using k-Means to determine the optimal
#' number of clusters.
#'
#' @param data
#' The dataset to be analyzed
#'
#' @param n
#' Number of observations to generate. Defaults to 100. Note: If n does not
#' split evenly by desired_k, it will be rounded up
#'
#' @param min_k
#' The starting number of clusters (k) for evaluation. Defaults to 2.
#'
#' @param max_k
#' The ending number of clusters (k) for evaluation. Defaults to 5.
#'
#' @param subsample_prop
#' Proportion of the dataset to use for each resample. Defaults to 0.8.
#'
#' @param num_subsamples
#' Number of resamples to generate. Defaults to 10.
#'
#' @param start_seed
#' Random seed for reproducibility. Defaults to 123.
#'
#' @param cluster_method
#' Specified clustering algorithm (i.e. "kmeans")
#'
#' @returns
#' A data frame where each row corresponds to a tested k cluster. The data frame
#' includes the similarity score, runtime, and a best_clust column, which is
#' blank for all rows except the one with the lowest similarity score, where it
#' is marked as "BEST".
#'
#' @export
#'
#' @examples
#' data <- simulate_mvn(n = 10)
#' crab(data = data, max_k = 4, cluster_method = "kmeans")

crab <- function(data,
                 n = 100,
                 min_k = 2,
                 max_k = 5,
                 subsample_prop = 0.8,
                 num_subsamples = 10,
                 start_seed = 123,
                 cluster_method){

  ### INPUT CHECKS:
  # data is a dataframe
  if (!is.data.frame(data)) {
    stop("Data must be in a dataframe.")}

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

  # start seed is a positive int
  if (start_seed %% 1 != 0 || start_seed < 0) {
    stop("The seed value must be an integer.")}

  ### FUNCTION:
  # intialize score list
  k_scores <- data.frame(k = numeric(),
                         Score = numeric(),
                         `Time (seconds)` = numeric(),
                         `Best Cluster (k)` = character(),
                         check.names = FALSE)

  # iterate over k values
  for (k in min_k:max_k){

    # start time
    tictoc::tic()

    # get buddies matrices and overall mean matrix
    mean_mat <- mean_matrix(data = data,
                            k = k,
                            subsample_prop = subsample_prop,
                            num_subsamples = num_subsamples,
                            start_seed = start_seed,
                            cluster_method = cluster_method)

    # store all scores
    score <- similarity_score(mean_matrix = mean_mat)

    # end time
    time_info <- tictoc::toc(quiet = TRUE)

    # calculate execution time
    elapsed_time <- time_info$toc - time_info$tic

    # add to results
    k_scores <- rbind(k_scores,
                      data.frame(k = k,
                                 Score = score,
                                 `Time (seconds)` = elapsed_time,
                                 `Best Cluster (k)` = " ",
                                 check.names = FALSE,
                                 stringsAsFactors = FALSE))}

  # find row with lowest score
  best_score_idx <- which.min(k_scores$Score)
  k_scores$`Best Cluster (k)`[best_score_idx] <- "BEST"

  # remove row names index
  rownames(k_scores) <- NULL

  # get similarity score
  return(k_scores)}
