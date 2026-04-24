#' Compute Contribution Score of Each Point in Relation to the CRAB Score.
#'
#' @param data
#' The dataset to be analyzed
#'
#' @param mean_matrix
#' The output list produced by mean_matrix()
#'
#' @returns
#' Generates a data frame containing the index, individual point score, and
#' individual point contribution score for each data point with respect to the
#' CRAB score. Higher contribution values increase the CRAB score, while values
#' near zero have minimal impact.
#'
#' @export
#'
#' @examples
#' # data frame
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
#' mvn_contributions <- point_contribution(data = mvn_df, mean_matrix = mean_mat_mvn)
#' head(mvn_contributions)
#'
#' # visualize
#' mvn_df |>
#' ggplot(aes(x = X1, y = X2,
#'            color = mvn_contributions$singlePointContribution)) +
#'            scale_color_gradient(low = "darkblue", high = "red") +
#'            geom_point() +
#'            theme_minimal() +
#'            labs(title = "Original Data with Point Contribution",
#'                 color = "Point Contribution")

point_contribution <- function(data, mean_matrix) {
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

  # data is a dataframe
  if (!is.data.frame(data)) {
    stop("Data must be in a dataframe.")}

  ### FUNCTION:
  # loop through every point
  scores <- lapply(c(1:nrow(data)), function(i) {

    # abs vals of i
    full_vec <- get_pointvec(i, mean_matrix$score)

    # computer score
    singlePointScore <- mean((1 - full_vec)^2)
    return(list(index = i, singlePointScore = singlePointScore))})

  # create data frame
  final_df <- do.call(rbind.data.frame, scores)

  # mean score
  total_score <- mean(final_df$singlePointScore)

  # sigmoid fn
  sigmoid <- function(x) 1 / (1 + exp(-x))

  # point contribution
  final_df <- final_df |>
    mutate(singlePointContribution = sigmoid(singlePointScore / total_score))

  return(final_df)}
