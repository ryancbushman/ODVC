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
#' M <- one_way_cov_B(error = 1, tau = 1, a = 10, n = 20)
#' score <- D_crit(M)
#'
D_crit <- function(M){
  D.score <- det(M)
  return(D.score)
}
