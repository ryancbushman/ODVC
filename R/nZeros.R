#' nZeros is a helper function that is used with getCols() to select only
#' columns from a dataframe that has a certain number of zeros
#'
#' @param X a dataframe that is being subset
#' @param N the number of experiment observations DOUBLE CHECK
#'
#' @return the number of zeros in a column
#' @export
nZeros <- function(X, N) {
  N - sum(X == 0)
}
