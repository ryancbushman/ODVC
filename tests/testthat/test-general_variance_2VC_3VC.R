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

test_that("One-way balanced information matrix is correct", {
  # This test is based on the balanced case of a 1-way nested random effects
  # model. See Searle, S. R., George Casella, and Charles E. McCulloch. Variance
  #' Components. Wiley Series in Probability and Statistics. Hoboken, NJ:
  #' Wiley, 2006." page 86.
  testthat::expect_equal(one_way_cov_B(error_sq = 1, tau = 1, a = 3, n = 5),
               general_variance_2VC(N = 15, n = 5, a = 3,
                                    sig_a_sq = 1, error_sq = 1))
})

test_that("One-way unbalanced information matrix is correct", {
  # This test is based on the unbalanced case of a 1-way nested random effects
  # model. See Searle, S. R., George Casella, and Charles E. McCulloch. Variance
  #' Components. Wiley Series in Probability and Statistics. Hoboken, NJ:
  #' Wiley, 2006." page 89.
  testthat::expect_equal(one_way_cov_U(N = 15, a = 3, n_i = c(4, 5, 6),
                                       sig_a_sq = 1, error_sq = 1),
                         general_variance_2VC(N = 15, n = c(4, 5, 6), a = 3,
                                              sig_a_sq = 1, error_sq = 1))
})

test_that("Two-way balanced information matrix is correct", {
  # This test is based on the balanced case of a 2-way nested random effects
  # model. See Searle, S. R., George Casella, and Charles E. McCulloch. Variance
  # Components. Wiley Series in Probability and Statistics. Hoboken, NJ:
  # Wiley, 2006." page 158.
  # Note: The book orders the variance components differently.
  tw_1 <- verify_2_way_info(n = 2, a = 2, b = 2, sig_a_sq = 1,
                            sig_b_sq = 1, error_sq = 1)
  tw_2 <- general_variance_3VC(N = 8, n_i_dot = c(4, 4), n_ij = c(2, 2, 2, 2),
                               sig_a_sq = 1, sig_b_sq = 1, error_sq = 1)
  testthat::expect_equal(tw_1[1, 1], tw_2[3, 3])
  testthat::expect_equal(tw_1[1, 2], tw_2[1, 3])
  testthat::expect_equal(tw_1[2, 1], tw_2[3, 1])
  testthat::expect_equal(tw_1[2, 2], tw_2[1, 1])
  testthat::expect_equal(tw_1[1, 3], tw_2[2, 3])
  testthat::expect_equal(tw_1[3, 1], tw_2[3, 2])
  testthat::expect_equal(tw_1[2, 3], tw_2[1, 2])
  testthat::expect_equal(tw_1[3, 2], tw_2[2, 1])
  testthat::expect_equal(tw_1[3, 3], tw_2[2, 2])
})

test_that("Two-way unbalanced information matrix is correct", {
  # This test is based on a check included in Searle 1970.
  # Large Sample Variances of Maximum Likelihood Estimators of Variance Components
  # Using Unbalanced Data
  # Author(s): S. R. Searle
  # Source: Biometrics , Sep., 1970, Vol. 26, No. 3 (Sep., 1970), pp. 505-524
  # Published by: International Biometric Society
  # Stable URL: https://www.jstor.org/stable/2529105
  sig_a_sq <- 1
  sig_b_sq <- 1
  error_sq <- 1

  n_11 <- 2
  n_12 <- 3
  n_21 <- 1
  n_22 <- 2
  n_23 <- 1

  n_dotdot <- 9
  c_dot <- 5

  m_11 <- (n_11 * sig_b_sq) + error_sq
  m_12 <- (n_12 * sig_b_sq) + error_sq
  m_21 <- (n_21 * sig_b_sq) + error_sq
  m_22 <- (n_22 * sig_b_sq) + error_sq
  m_23 <- (n_23 * sig_b_sq) + error_sq

  A_111 <- (n_11 / m_11) + (n_12 / m_12)
  A_211 <- (n_21 / m_21) + (n_22 / m_22) + (n_23 / m_23)
  A_122 <- (n_11^2 / m_11^2) + (n_12^2 / m_12^2)
  A_222 <- (n_21^2 / m_21^2) + (n_22^2 / m_22^2) + (n_23^2 / m_23^2)
  A_112 <- (n_11 / m_11^2) + (n_12 / m_12^2)
  A_212 <- (n_21 / m_21^2) + (n_22 / m_22^2) + (n_23 / m_23^2)
  A_133 <- (n_11^3 / m_11^3) + (n_12^3 / m_12^3)
  A_233 <- (n_21^3 / m_21^3) + (n_22^3 / m_22^3) + (n_23 / m_23^3)
  A_123 <- (n_11^2 / m_11^3) + (n_12^2 / m_12^3)
  A_223 <- (n_21^2 / m_21^3) + (n_22^2 / m_22^3) + (n_23^2 / m_23^3)
  A_102 <- (n_11^0 / m_11^2) + (n_12^0 / m_12^2)
  A_202 <- (n_21^0 / m_21^2) + (n_22^0 / m_22^2) + (n_23^0 / m_23^2)
  A_113 <- (n_11 / m_11^3) + (n_12 / m_12^3)
  A_213 <- (n_21 / m_21^3) + (n_22 / m_22^3) + (n_23 / m_23^3)

  q_1 <- 1 + (sig_a_sq * A_111)
  q_2 <- 1 + (sig_a_sq * A_211)

  t_aa <- (A_111^2 / q_1^2) + (A_211^2 / q_2^2)
  t_ab <- (A_122 / q_1^2) + (A_222 / q_2^2)
  t_ae <- (A_112 / q_1^2) + (A_212 / q_2^2)
  t_bb <- (A_122 - ((2 * sig_a_sq * A_133) / q_1) + ((sig_a_sq^2 * A_122^2) / q_1^2)) +
    (A_222 - ((2 * sig_a_sq * A_233) / q_2) + ((sig_a_sq^2 * A_222^2) / q_2^2))
  t_be <- (A_112 - ((2 * sig_a_sq * A_123) / q_1) + ((sig_a_sq^2 * A_112 * A_122) / q_1^2)) +
    (A_212 - ((2 * sig_a_sq * A_223) / q_2) + ((sig_a_sq^2 * A_212 * A_222) / q_2^2))
  t_ee <- (A_102 - ((2 * sig_a_sq * A_113) / q_1) + ((sig_a_sq^2 * A_112^2) / q_1^2)) +
    (A_202 - ((2 * sig_a_sq * A_213) / q_2) + ((sig_a_sq^2 * A_212^2) / q_2^2)) + ((n_dotdot - c_dot)/error_sq^2)

  elements = c(t_aa, t_ab, t_ae,
               t_ab, t_bb, t_be,
               t_ae, t_be, t_ee)

  test <- 2 * solve(matrix(elements, 3, 3))

  testthat::expect_equal(test,
                         general_variance_3VC(9, c(5, 4), c(2, 3, 1, 2, 1),
                                                    1, 1, 1))
})
