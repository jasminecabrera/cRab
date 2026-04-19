#' Creating Buddies Matricies
#'
#' @description
#' Generates buddies matrix for each resample corresponding to a specified k.
#' The number of matrices produces is equivalent to the number of subsamples. In
#' these matrices, rivals are assigned a value of -1 and buddies are assigned a
#' value of 1. Rivals belong to different clusters, while buddies belong to the
#' same cluster. The function applies the k-means algorithm via the k_means()
#' function.
#'
#' @param data
#' The dataset to be analyzed
#'
#' @param k
#' The specific number of clusters (k) to evaluate. Defaults to 3.
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
#' Produces a list of matrices, where the number of matrices
#' corresponds to num_subsamples.
#'
#'
#' @export
#'
#' @examples
#' data <- simulate_mvn(n = 10)
#' buddy <- buddies_matrix(data = data, cluster_method = "kmeans")
#' buddy[[1]]
#' buddy[[2]]

buddies_matrix <- function(data = data,
                           k = 3,
                           subsample_prop = 0.8,
                           num_subsamples = 10,
                           start_seed = 123,
                           cluster_method){

  ### INPUT CHECKS:
  # data is a dataframe
  if (!is.data.frame(data)) {
    stop("Data must be in a dataframe.")}

  # k is a positive int
  if (k %% 1 != 0 || k < 1) {
    stop("The desired k value must be a positive integer.")}

  # k is 1
  if (k == 1) {
    warning("The desired k value is 1 (one cluster).")}

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
  # parallelization
  cores <- max(1, parallel::detectCores() - 5)
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))

  # drop na and scale data
  data_clean <- data |>
    tidyr::drop_na() |>
    scale() |>
    as.data.frame() |>
    dplyr::mutate(index = dplyr::row_number())

  # initialize results list -- holds matrix for a given k
  results <- list()
  results <- foreach::`%dopar%`(foreach::foreach(i = 1:num_subsamples,
                                                 .packages = c("tidyclust",
                                                               "tidyverse",
                                                               "tidymodels")), {

                                                                 # initalizing blank matrix
                                                                 result_matrix <- matrix(0, nrow = nrow(data_clean),
                                                                                         ncol = nrow(data_clean))

                                                                 # seed
                                                                 set.seed(start_seed + i)

                                                                 # resample
                                                                 random_sample <- data_clean |>
                                                                   filter(index %in% sample(index, floor(subsample_prop * nrow(data))))

                                                                 # clustering
                                                                 if (cluster_method == "kmeans"){
                                                                   model <- tidyclust::k_means(num_clusters = k) |>
                                                                     tidyclust::fit(~ .,
                                                                                    data = random_sample[, !(names(random_sample) %in% "index")])}

                                                                 else{stop("Model option currently in progress.")}


                                                                 # assign points to cluster
                                                                 intermediate <- data.frame(random_sample$index,
                                                                                            tidyclust::extract_cluster_assignment(model) |>
                                                                                              mutate(.cluster = as.character(.cluster)),
                                                                                            stringsAsFactors = FALSE)

                                                                 # rename columns
                                                                 colnames(intermediate) <- c("index", "cluster")

                                                                 # assign results matrix (-1, 1, 0)
                                                                 for (c in unique(intermediate$cluster)){
                                                                   idx <- intermediate[intermediate$cluster == c, ]$index

                                                                   if (length(idx) > 1){
                                                                     idx <- sort(unlist(idx), method = "radix")
                                                                     ones <- t(combn(idx, 2))
                                                                     result_matrix[ones[, 1], ones[, 2]] <- 1}

                                                                   neg_one_idx <- expand.grid(idx, setdiff(random_sample$index, idx))
                                                                   result_matrix[neg_one_idx[, 1], neg_one_idx[, 2]] <- -1}

                                                                 # set lower triangle and diagonal to NA
                                                                 result_matrix[lower.tri(result_matrix, diag = TRUE)] <- NA

                                                                 return(result_matrix)})

  # return results
  return(results)}
