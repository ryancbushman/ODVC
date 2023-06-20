#' one_way_cov_U
#'
#' one_way_cov_U provides the asymptotic covariance matrix for the maximum
#' likelihood estimators of an unbalanced one-way ANOVA experiment design with a
#' single random effect. This matrix represents the inverse of the information
#' matrix of the user's model and should be minimized in optimal design of
#' experiments.
#'
#' @param N an integer number of total design points in the experiment
#' @param a an integer number of groups in the experiment
#' @param n_i a vector of doubles representing the number of replications per group
#' @param vc a double representing the estimate of the variance component
#' @param error a double representing the residual mean error
#'
#' @return the maximum likelihood estimator covariance matrix for a user
#' specified unbalanced one-way ANOVA model.
#' @export
#'
#' @examples
#'
#' u_info <- u_infomat(N = 10, a = 4, n_i = c(1,2,3,4), vc = 2, error = 1)
#'
one_way_cov_U <- function(N, a, n_i, vc, error) {
  lambda_i <- error + n_i * vc
  D <- ((N - a) / error^2) * sum((n_i / lambda_i)^2) +
    sum(1 / lambda_i^2) * sum((n_i / lambda_i)^2) -
    (sum(n_i / lambda_i^2))^2
  tl <- sum((n_i / lambda_i)^2)
  tr <- - sum(n_i / lambda_i^2)
  bl <- tr
  br <- ((N - a) / error^2) + sum(1 / lambda_i^2)
  elements <- c(tl, tr, bl, br)
  info <- (2 / D) * matrix(elements, 2, 2, byrow = TRUE)
  return(info)
}
