#' one_way_cov_B
#'
#' one_way_cov_B provides the asymptotic covariance matrix for
#' the maximum likelihood estimators of a one-way ANOVA experiment design with a
#' single random effect. This matrix represents the inverse of the information
#' matrix of the user's model and should be minimized in optimal design of
#' experiments.
#'
#' @param error the residual error variance component
#' @param tau a double representing the ratio of the random effect variance
#' component and the residual error variance component \deqn{\frac{\sigma^2_\alpha}{\sigma^2}}
#' @param a an integer representing the number of groups in a one-way ANOVA
#' experiment design
#' @param n an integer number of replications per group in a one-way ANOVA
#' experiment design
#'
#' @return the maximum likelihood estimator covariance matrix for a user
#' specified balanced one-way ANOVA model.
#' @export
#'
#' @examples
#'
#' M <- one_way_cov_B(error = 1, tau = 1, a = 5, n = 5)
#'
one_way_cov_B <- function(error = 1, tau = 1, a, n) {
  tl <- 1 / (a * (n - 1))
  tr <- -1 / ((a * n) * (n - 1))
  bl <- tr
  br <- (1 / n^2) * (((1 + (n * tau))^2 / a) + (1 / (a * (n - 1))))
  elements <- c(tl, tr, bl, br)
  info <- (2 * (error)^2) * matrix(elements, nrow = 2, ncol = 2, byrow = TRUE)
  return(info)
}
