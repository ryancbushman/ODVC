#' A_crit
#'
#' A_crit returns the A score for a specific experiment design. The A score is
#' the trace of whatever matrix is being optimized.
#'
#' @param M a matrix object
#'
#' @return the A score of the input matrix
#' @export
#'
#' @examples
#'
#' M <- general_variance_2VC(N = 20, n = 4, a = 5, sig_a_sq = 1, error_sq = 1)
#' score <- A_crit(M)
#'
A_crit <- function(M) {
  A.score <- sum(diag(M))
  return(A.score)
}
