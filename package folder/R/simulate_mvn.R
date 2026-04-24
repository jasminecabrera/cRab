#' Simulate Data from a Multivariate Normal Distribution
#'
#' @description
#' Generates a simulated dataset from a multivariate normal (MVN) distribution
#'
#' @param n Number of observations to generate. Defaults to 100. Note: If n does
#' not split evenly by desired_k(), it will be rounded up.
#'
#' @param variance Variance value used to construct the covariance matrix.
#' Defaults to 0.7
#'
#' @param desired_k Desired number of clusters k. Defaults to 3.
#'
#' @param start_seed Random seed for reproducibility. Defaults to 123.
#'
#' @returns n x 2 standardized list with columns V1 and V2. Standardization is
#' performed within the function.
#'
#' @export
#'
#' @examples simulate_mvn()
#' @examples simulate_mvn(n = 150, covariance = 0.4)
#' @examples simulate_mvn(desired_k = 4)

simulate_mvn <- function(n = 100,
                         variance = 0.4,
                         desired_k = 3,
                         start_seed = 123){

  ### INPUT CHECKS:
  # n is an int
  if (n %% 1 != 0 || n <= 0) {
    stop("The number of observations, n, must be a positive integer.")}

  # variance is greater than 0
  if (variance <= 0) {
    stop("Variance must be greater than 0.")}

  # desired_k is an positive int
  if (desired_k %% 1 != 0 || desired_k < 1) {
    stop("The desired k value must be a positive integer.")}

  # desired_k is 1
  if (desired_k == 1) {
    warning("The desired k value is 1 (one cluster).")}

  # start seed is a positive int
  if (start_seed %% 1 != 0 || start_seed < 0) {
    stop("The seed value must be an integer.")}

  ### FUNCTION:
  set.seed(start_seed)

  # creating covariance matrix
  cov_matrix <- diag(2) * variance

  # obs per cluster
  n_per_cluster <- ceiling(n / desired_k)

  # distance between each cluster center
  separation <- 3

  # create grid coordinates for means
  grid_coords <- expand.grid(x = seq(0, by = separation,
                                     length.out = ceiling(sqrt(desired_k))),
                             y = seq(0, by = separation,
                                     length.out = ceiling(sqrt(desired_k))))

  # pick first k grid points
  centers <- grid_coords[1:desired_k, ]

  # generate data
  sim_data <- do.call(rbind, lapply(1:desired_k, function(k){

    # determine mean
    mu <- c(centers$x[k], centers$y[k])

    # generate clusters
    MASS::mvrnorm(n = n_per_cluster,
                  mu = mu,
                  Sigma = cov_matrix)})) |>
    as.data.frame() |>
    setNames(c("X1", "X2"))

  sim_data}
