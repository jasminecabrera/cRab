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
#' mean_matrix <- mean_matrix(data = data, k = 3, subsample_prop = 0.8,
#' num_subsamples = 3, cluster_method = "kmeans")
#' similarity_score(mean_matrix)

similarity_score <- function(mean_matrix = mean_mat){

  ### INPUT CHECKS:
  # check if list
  if (!is.list(mean_matrix)) {
    stop("Input must be a list (output from mean_matrix function).")}

  # correct element names
  required_names <- c("matrices", "score")
  if (!all(required_names %in% names(mean_matrix))) {
    stop("Input list is missing 'matrices' or 'score'. Did you provide the correct object?")}

  # check score is a matrix
  if (!is.matrix(mean_matrix$score)) {
    stop("The 'score' element must be a matrix.")}

  ### FUNCTION:
  # get score matrix
  mean_matrix_score <- mean_matrix$score

  # transforms into vector
  result_vector <- as.vector(mean_matrix_score)

  # keeps only non NA values
  result_vector <- abs(result_vector[!is.na(result_vector)])

  # finds squared distance from one
  return(mean((1 - result_vector)**2))}
