#' Simulate Data from a Multivariate Normal Distribution
#'
#' @description
#' Generates a simulated dataset from a multivariate normal (MVN) distribution
#'
#' @param n Number of observations to generate.
#'
#'          Note: If n does not split evenly
#'          by desired_k(), it will be rounded up.
#'
#'          Defaults to 100

#' @param covariance Variance value used to construct the covariance matrix
#'
#'          Defaults to 0.7
#'
#' @param desired_k Desired number of clusters k
#'
#'          Defaults to 3
#'
#' @param start_seed Random seed for reproducibility
#'
#'          Defaults to 123
#'
#' @returns n x 2 standardized list with columns V1 and V2. Standardization is
#'          performed within the function.
#'
#' @export
#'
#' @examples simulate_mvn()
#' @examples simulate_mvn(n = 150, covariance = 0.4)
#' @examples simulate_mvn(desired_k = 4)

simulate_mvn <- function(n = 100,
                         covariance = 0.4,
                         desired_k = 3,
                         start_seed = 123){

  set.seed(start_seed)

  # creating covariance matrix
  cov_matrix <- diag(2) * covariance

  # if desired k is wanted
  if (!is.null(desired_k) && desired_k > 0)  {

    # obs per cluster
    n_per_cluster <- ceiling(n / desired_k)

    # distance between each cluster center
    separation <- 3

    # create grid coordinates for means
    grid_coords <- base::expand.grid(x = seq(0,
                                       by = separation,
                                       length.out = ceiling(sqrt(desired_k))),
                               y = seq(0,
                                       by = separation,
                                       length.out = ceiling(sqrt(desired_k))))

    # pick first k grid points
    centers <- grid_coords[1:desired_k, ]

    # generate data
    data <- base::do.call(rbind, lapply(1:desired_k, function(k){

      # determine mean
      mu <- c(centers$x[k], centers$y[k])

      # generate clusters
      MASS::mvrnorm(n = n_per_cluster,
                    mu = mu,
                    Sigma = cov_matrix)}))


    # if no desired kvalue
  }else{
    # mu
    mu <- c(0, 0)

    # data
    data <- MASS::mvrnorm(n = n,
                          mu = mu,
                          Sigma = cov_matrix)}

  # change to dataframe
  data <- as.data.frame(data)

  # standardize
  data <- data |>
    dplyr::mutate(across(c(V1, V2), ~ (. - mean(.)) / sd(.)))

  # change col names
  colnames(data) <- c("V1", "V2")

  return(data)}
