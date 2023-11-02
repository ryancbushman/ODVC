#' verify_2_way_info is a helper function that calculates the covariance matrix
#' of maximum likelihood estimators of a balanced two-way nested random effects model.
#' See "Searle, S. R., George Casella, and Charles E. McCulloch. Variance
#' Components. Wiley Series in Probability and Statistics. Hoboken, NJ:
#' Wiley, 2006." page 158.
#'
#' @param n integer number of replications per sub-group
#' @param a integer number of groups
#' @param b integer number of sub-groups per group
#' @param sig_a_sq estimate of \deqn{\sigma^2_A}
#' @param sig_b_sq estimate of \deqn{\sigma^2_B}
#' @param error_sq estimate of \deqn{\sigma^2}
#'
#' @return a matrix object that is the covariance matrix of the maximum
#' likelihood estimators for the variance components of the model.
#' @export
#'
#' @examples
#' #Balanced Design
#' verify_2_way_info(n = 2, a = 2, b = 2, sig_a_sq = 1, sig_b_sq = 1, error_sq = 1)
#'
verify_2_way_info <- function(n, a, b, sig_a_sq, sig_b_sq, error_sq) {
  delta <- (error_sq + n * sig_b_sq)^2 / (b - 1)
  theta <- error_sq + (n * sig_b_sq) + (n * b * sig_a_sq)
  elem <- c(error_sq^2 / (b * (n - 1)),
            0,
            -error_sq^2 / (b * n * (n-1)),
            0,
            (delta + theta^2) / (b^2 * n^2),
            -delta / (b * n^2),
            -error_sq^2 / (b * n * (n-1)),
            -delta / (b * n^2),
            (1 / n^2) * (delta + (error_sq^2 / (b * (n - 1)))))
  info <- (2 / a) * matrix(elem, nrow = 3, ncol = 3, byrow = TRUE)
  return(info)
}
