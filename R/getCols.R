#' Title
#'
#' @param P
#' @param A
#' @param N
#'
#' @return
#' @export
#'
#' @examples
getCols <- function(P, A, N){
  n.lev.a   <- apply(P, 2, nZeros, N = N)
  keep.cols <- which(n.lev.a == A)
  return(P[,keep.cols])
}
