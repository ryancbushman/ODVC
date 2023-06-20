#' Title
#'
#' @param X
#' @param N
#'
#' @return
#' @export
#'
#' @examples
nZeros <- function(X, N) {
  N - sum(X == 0)
}
