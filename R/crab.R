#' Calling cRab
#'
#' @description
#' Computes cluster score for clusters using k-Means to determine the optimal
#' number of clusters
#'
#' @param data
#' The dataset to be analyzed
#'
#' @param n
#' Number of observations to generate
#'
#' Note: If n does not split evenly by desired_k, it will be rounded up
#'
#' Defaults to 100
#'
#' @param min_k
#' The starting number of clusters (k) for evaluation
#'
#' Defaults to 2
#'
#' @param max_k
#' The ending number of clusters (k) for evaluation
#'
#' Defaults to 5
#'
#' @param subsample_prop
#' Proportion of the dataset to use for each resample
#'
#' Defaults to 0.8
#'
#' @param num_subsamples
#' Number of resamples to generate
#'
#' Defaults to 10
#'
#' @param start_seed
#' Random seed for reproducibility
#'
#' Defaults to 123
#'
#' @param cluster_method
#' Specified clustering algorithm
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

  # intialize score list
  k_scores <- data.frame(k = numeric(),
                         score = numeric(),
                         time = numeric(),
                         best_clust = character())

  # iterate over k values
  for (k in min_k:max_k){

    # start time
    tictoc::tic()

    # get buddies matrices and overall mean matrix
    mean_matrix <- mean_matrix(data = data,
                               k = k,
                               subsample_prop = subsample_prop,
                               num_subsamples = num_subsamples,
                               start_seed = start_seed,
                               cluster_method = cluster_method)

    # store all scores
    score <- similarity_score(mean_matrix = mean_matrix)

    # end time
    time_info <- tictoc::toc(quiet = TRUE)

    # calculate execution time
    elapsed_time <- time_info$toc - time_info$tic

    # add to results
    k_scores <- rbind(k_scores,
                      data.frame(k = k,
                                 score = score,
                                 time = elapsed_time,
                                 best_clust = ""))}

  # find row with lowest score
  best_score_idx <- which.min(k_scores$score)
  k_scores$best_clust[best_score_idx] <- "BEST"

  # get similarity score
  return(k_scores)}
