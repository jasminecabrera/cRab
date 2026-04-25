#' Initializing crab_setup with crab_prep
#'
#' @description
#' Acts as a structured container that holds the dataset, the original columns,
#' while reserving space for user-defined columns and parameters that will later
#' be used by crab_bake().
#'
#' @param data
#' The dataset to be analyzed
#'
#' @returns
#' A list containing the dataset, the original column names, and placeholder NULL
#' values for user-specified columns and parameters.
#'
#' @export
#'
#' @examples
#' palmerpenguins::penguins |> crab_prep()


crab_prep <- function(data){

  ### INPUT CHECKS:
  if (!is.data.frame(data)) {
    stop("Data must be in a dataframe.")}

  ### FUNCTION:
  # grab all columns, leave user choices NULL
  data_info <- list(data = data,
                    original_cols = colnames(data),
                    specified_cols = NULL,
                    specified_params = list())

  return(data_info)}
