#' A_crit
#'
#' A_crit returns the A score for a specific experiment design. The A score is
#' the trace of whatever matrix is being optimized.
#'
#' @param M a matrix object
#'
#' @return the A score of the input provided matrix
#' @export
#'
#' @examples
#'
#' M <- one_way_cov_B(error = 1, tau = 1, a = 10, n = 20)
#' score <- A_crit(M)
#'
A_crit <- function(M) {
  A.score <- sum(diag(M))
  return(A.score)
}
