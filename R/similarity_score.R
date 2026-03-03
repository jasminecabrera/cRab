#' Obtain Similarity Score
#'
#' @description
#' Computes a similarity score representing the squared distance from 1.
#'
#' @param mean_matrix
#' The output list produced by mean_matrix()
#'
#' @returns
#' Returns a single numeric value representing how closely the clustering results
#' align with the desired number of k clusters, based on the consistency of the
#' 1s and -1s in the score matrix.
#'
#' @export
#'
#' @examples
#' data <- simulate_mvn(n = 10)
#' mean_matrix <- mean_matrix(data = data, k = 3, subsample_prop = 0.8, num_subsamples = 3)
#' similarity_score(mean_matrix)

similarity_score <- function(mean_matrix = mean_matrix){

  # get score matrix
  mean_matrix_score <- mean_matrix$score

  # transforms into vector
  result_vector <- as.vector(mean_matrix_score)

  # keeps only non NA values
  result_vector <- abs(result_vector[!is.na(result_vector)])

  # finds squared distance from one
  return(sum((1 - result_vector)**2))}
