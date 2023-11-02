#' getCols selects the columns from a dataframe of all partitions of N such that
#' all the selected partitions have the same number of groups.
#'
#' @param P partitions of N observations into A groups
#' @param A an integer representing the number of groups
#' @param N an integer representing the total number of experiment observations
#'
#' @return a dataframe of ways to split up N observations into A groups
#' @export
#'
#' @examples
#' nparts <- getCols(P = parts(30), A = 5, N = 30)
getCols <- function(P, A, N){
  n.lev.a   <- apply(P, 2, nZeros, N = N)
  keep.cols <- which(n.lev.a == A)
  return(P[1:A,keep.cols])
}
