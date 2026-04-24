#' Grab all pairwise comparisons for a given observation i and all other observations j in the mean matrix.
#'
#' @param i
#' Index of a point in dataset
#'
#' @param mean_matrix_score
#' The output score element produced by mean_matrix() (accessed by
#' mean_matrix$score).
#'
#' @returns
#' Generates a list of all of the pairwise comparisons between observations i and
#' every other observation j in the mean matrix.
#'
#' @export
#'
#' @examples
#' # simulated data
#' mvn_df <- simulate_mvn(n = 400, variance = 0.4, desired_k = 3)
#'
#' # mean matrix
#' mean_mat_mvn <- mean_matrix(data = mvn_df,
#'                             k = 3,
#'                             subsample_prop = 0.8,
#'                             num_subsamples = 10,
#'                             cluster_method = "kmeans")
#'
#' # get point contributions
#' get_pointvec(i = 67, mean_matrix_score = mean_mat_mvn$score)

get_pointvec <- function(i, mean_matrix_score) {
  ### INPUT CHECKS:

  # check score is a matrix
  if (!is.matrix(mean_matrix_score)) {
    stop("The 'score' element must be a matrix.")}

  # check i is an integer
  if (i %% 1 != 0 || i < 1) {
    stop("Point must be a positive integer.")}

  ### FUNCTION:
  # grab i row and col, without diagonal
  row <- mean_matrix_score[i, -i]
  col <- mean_matrix_score[-i, i]

  # grab all abs vals of i, removing NAs
  res_vec <- c(row, col)
  res_vec <- abs(res_vec[!is.na(res_vec)])
  return(res_vec)}
