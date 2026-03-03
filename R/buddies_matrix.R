#' Creating Buddies Matricies
#'
#' @description
#' Generates buddies matrix for each resample corresponding to a specified k.
#' The number of matrices produces is equivalent to the number of subsamples.
#' In these matrices, rivals are assigned a value of -1 and buddies are assigned
#' a value of 1. Rivals belong to different clusters, while buddies belong to
#' the same cluster. The function applies the k-means algorithm via the k_means()
#' function.
#'
#' @param data
#' The dataset to be analyzed
#'
#' @param k
#' The specific number of clusters (k) to evaluate
#'
#' Defaults to 3
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

  cl <- parallel::makeCluster(max(1, parallel::detectCores() - 2))
  doParallel::registerDoParallel(cl)

  # drop na
  data <- data |>
    tidyr::drop_na() |>
    base::scale() |>
    as.data.frame()

  # add index
  data$index <- 1:nrow(data)

  # initialize results list -- holds matrix for each subsample in each k
  results <- list()

  `%dopar%` <- foreach::`%dopar%`

  results <- foreach::foreach(i = 1:num_subsamples,
                     .packages = c("tidyclust",
                                   "tidyverse",
                                   "tidymodels")) %dopar% {

                                     # initalizing blank matrix
                                     result_matrix <- matrix(0,
                                                             nrow = nrow(data),
                                                             ncol = nrow(data))

                                     # seed
                                     set.seed(start_seed + i)

                                     # resample
                                     random_sample <- data |>
                                       filter(index %in% sample(index, subsample_prop * nrow(data)))

                                     if (cluster_method != "kmeans"){
                                       stop("Error: Clustering method does not currently work.
                                            Please change to 'kmeans'.")}

                                     # kmeans
                                     else {kmodel <- k_means(num_clusters = k) |>
                                       fit(~ random_sample[[1]] + random_sample[[2]],
                                           data = random_sample)}


                                     # assign points to cluster
                                     intermediate <- data.frame(random_sample$index,
                                                                extract_cluster_assignment(kmodel) |>
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


                                     return(result_matrix)}

  on.exit(parallel::stopCluster(cl), add = TRUE)

  # return results
  return(results)}
