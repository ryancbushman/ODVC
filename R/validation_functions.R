#' one_way_cov_B
#'
#' one_way_cov_B provides the asymptotic covariance matrix for
#' the maximum likelihood estimators of a one-way ANOVA experiment design with a
#' single random effect. This matrix represents the inverse of the information
#' matrix of the user's model and should be minimized in optimal design of
#' experiments.
#'
#' @param error_sq the residual error variance component
#' @param tau a double representing the ratio of the random effect variance
#' component and the residual error variance component \deqn{\frac{\sigma^2_\alpha}{\sigma^2}}
#' @param a an integer representing the number of groups in a one-way ANOVA
#' experiment design
#' @param n an integer number of replications per group in a one-way ANOVA
#' experiment design
#'
#' @return the maximum likelihood estimator covariance matrix for a user
#' specified balanced one-way ANOVA model.
#'
#'
#' @examples
#'
#' M <- one_way_cov_B(error_sq = 1, tau = 1, a = 5, n = 5)
#'
one_way_cov_B <- function(error_sq = 1, tau = 1, a, n) {
  tl <- 1 / (a * (n - 1))
  tr <- -1 / ((a * n) * (n - 1))
  bl <- tr
  br <- (1 / n^2) * (((1 + (n * tau))^2 / a) + (1 / (a * (n - 1))))
  elements <- c(tl, tr, bl, br)
  info <- (2 * (error_sq)^2) * matrix(elements, nrow = 2, ncol = 2, byrow = TRUE)
  return(info)
}

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
#' @param sig_a_sq a double representing the estimate of \deqn{\sigma^2_A}
#' @param error_sq a double representing the residual mean error
#'
#' @return the maximum likelihood estimator covariance matrix for a user
#' specified unbalanced one-way ANOVA model.
#'
#'
#' @examples
#'
#' information <- one_way_cov_U(N = 10, a = 4, n_i = c(1,2,3,4), sig_a_sq = 2, error_sq = 1)
#'
one_way_cov_U <- function(N, a, n_i, sig_a_sq, error_sq) {
  lambda_i <- error_sq + n_i * sig_a_sq
  D <- ((N - a) / error_sq^2) * sum((n_i / lambda_i)^2) +
    sum(1 / lambda_i^2) * sum((n_i / lambda_i)^2) -
    (sum(n_i / lambda_i^2))^2
  tl <- sum((n_i / lambda_i)^2)
  tr <- - sum(n_i / lambda_i^2)
  bl <- tr
  br <- ((N - a) / error_sq^2) + sum(1 / lambda_i^2)
  elements <- c(tl, tr, bl, br)
  info <- (2 / D) * matrix(elements, 2, 2, byrow = TRUE)
  return(info)
}

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
#'
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
