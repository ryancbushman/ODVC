#' D_crit
#'
#' D_crit returns the D score for a specific experiment design. The D score is
#' the determinant of whatever matrix is being optimized.
#'
#' @param M a matrix object
#'
#' @return the D score of the input provided matrix
#' @export
#'
#' @examples
#'
#' M <- general_variance_2VC(N = 20, n = 4, a = 5, sig_a_sq = 1, error_sq = 1)
#' score <- D_crit(M)
#'
D_crit <- function(M){
  D.score <- det(M)
  return(D.score)
}
