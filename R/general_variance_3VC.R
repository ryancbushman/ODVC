#' general_variance_3VC
#'
#' general_variance_3VC returns the MLE covariance matrix for optimization
#' in a nested ANOVA model with three variance components: \eqn{\sigma^2_A},
#' \eqn{\sigma^2_B}, and \eqn{\sigma^2}
#'
#' @param N an integer representing the total number of experimental runs
#' @param n_i_dot a vector of integers representing the total number of runs
#' at the A level of the model. The elements of the vector should sum to N
#' @param n_ij a vector of integers representing the number of runs per group
#' at the B level of the model. The elements of the vector should sum to N
#' @param sig_a_sq a double representing the estimate of \eqn{\sigma^2_A}
#' @param sig_b_sq a double representing the estimate of \eqn{\sigma^2_B}
#' @param error_sq a double representing the estimate of \eqn{\sigma^2}
#'
#' @return the covariance matrix of the model's maximum likelihood estimators
#' @export
#'
#' @examples
#' # Balanced design
#' general_variance_3VC(N = 8, n_i_dot = c(4, 4), n_ij = c(2, 2, 2, 2),
#' sig_a_sq = 0.5, sig_b_sq = 2, error_sq = 1)
#'
#' # Unbalanced design
#' general_variance_3VC(N = 9, n_i_dot = c(4, 5), n_ij = c(2, 3, 1, 2, 1),
#' sig_a_sq = 0.5, sig_b_sq = 2, error_sq = 1)
general_variance_3VC <- function(N, n_i_dot, n_ij, sig_a_sq, sig_b_sq, error_sq) {
  MLE_var <- matrix(rep(0, 9), nrow = 3, ncol = 3)
  J_a <- vector(mode = "list", length = length(n_i_dot))
  for (i in seq_along(n_i_dot)) {
    J_a[[i]] <- n_i_dot[i] * J_n(n_i_dot[i])
  }
  J_b <- vector(mode = "list", length = length(n_ij))
  for (j in seq_along(n_ij)) {
    J_b[[j]] <- n_ij[j] * J_n(n_ij[j])
  }
  derivatives <- list(do.call(statespacer::BlockMatrix, J_a),
                      do.call(statespacer::BlockMatrix, J_b), diag(1, N))
  temp_var <- V_3VC(N, n_i_dot, n_ij, sig_a_sq, sig_b_sq, error_sq)
  for (i in seq_along(derivatives)) {
    for (j in seq_along(derivatives)) {
      MLE_var[i, j] <- (1 / 2) * sum(diag(solve(temp_var) %*%
                                            derivatives[[j]] %*%
                                            solve(temp_var) %*%
                                            derivatives[[i]]))
    }
  }
  return(solve(MLE_var))
}

#' V_3VC
#'
#' V_3VC returns the variance matrix for a 2-way nested ANOVA model that has
#' a total of three variance components: \eqn{\sigma^2_A}, \eqn{\sigma^2_B},
#' and \eqn{\sigma^2}
#'
#' @param N an integer representing the total number of experimental runs
#' @param n_i_dot integers representing the total number of reps in each group at
#' the first level of the experiment
#' @param n_ij integers representing the total number of reps in each group at
#' the second level of the experiment
#' @param sig_a_sq a double representing the estimate of \eqn{\sigma^2_A}
#' @param sig_b_sq a double representing the estimate of \eqn{\sigma^2_B}
#' @param error_sq a double representing the estimate of \eqn{\sigma^2}
#'
#' @return the variance matrix of a 2-way nested anova model
#' @export
#'
#' @examples
#' # Balanced design
#' V_3VC(N = 8, n_i_dot = c(4, 4), n_ij = c(2, 2, 2, 2), sig_a_sq = 2, sig_b_sq = 3, error_sq = 1)
#'
#' # Unbalanced design
#' V_3VC(N = 9, n_i_dot = c(4, 5), n_ij = c(2, 3, 1, 2, 1), sig_a_sq = 2, sig_b_sq = 3, error_sq = 1)
V_3VC <- function(N, n_i_dot, n_ij, sig_a_sq, sig_b_sq, error_sq) {
  blocks_a <- vector(mode = "list", length = length(n_i_dot))
  for (i in seq_along(n_i_dot)) {
    blocks_a[[i]] <- n_i_dot[i] * J_n(n_i_dot[i]) # J_n defined in V_2VC.R
  }
  var_a <- do.call(statespacer::BlockMatrix, blocks_a)
  blocks_b <- vector(mode = "list", length = length(n_ij))
  for (j in seq_along(n_ij)) {
    blocks_b[[j]] <- n_ij[j] * J_n(n_ij[j])
  }
  var_b <- do.call(statespacer::BlockMatrix, blocks_b)
  var_e <- diag(1, N)
  var <- error_sq * var_e + sig_a_sq * var_a + sig_b_sq * var_b
  return(var)
}

####################### Balanced Verification ##################################

# Guess as to why order is mixed up: order of derivatives.
# Original was error, alpha, beta. Will test alpha, beta, error

# Verification for balanced design
# c changed to q to avoid confusion with concatenation function c()

# From Searle 1970, pg 514 (11 in pdf)
q = 2
n = 2
a = 2
sig_a_sq = 1
sig_b_sq = 1
error_sq = 1
x = (a * (q - 1)) / ((n * sig_b_sq) + error_sq)^2
y = a / ((q * n * sig_a_sq) + (n * sig_b_sq) + error_sq)^2
z = (a * q * (n - 1)) / error_sq

elements <- c(q^2 * n^2 * y,
              q * n^2 * y,
              q * n * y,
              q * n^2 * y,
              n^2 * (x + y),
              n * (x + y),
              q * n * y,
              n * (x + y),
              x + y + z)

test <- 2 * solve(matrix(elements, 3, 3))
round(test, 3)

# Searle 1992 pg 158 (180 in pdf)

# verify_2_way_info(2,2,2,1,1,1)

# Function I wrote using Searle's generating function

round(general_variance_3VC(N = 8, n_i_dot = c(4, 4), n_ij = c(2, 2, 2, 2), sig_a_sq = 1,
                     sig_b_sq = 1, error_sq = 1), 3)

# All 3 balanced design verifications match. Note Searle 1992 version orders the
# elements differently in the textbook, hence why they appear out of order
# compared to the other versions

########################## Unbalanced Verificaiton ############################

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

# sig_a_sq <- 1
# sig_b_sq <- 1
# error_sq <- 1
#
# n_11 <- 20
# n_12 <- 30
# n_21 <- 10
# n_22 <- 20
# n_23 <- 10
#
# n_dotdot <- 90
# c_dot <- 5
#
# sig_a_sq <- 1
# sig_b_sq <- 1
# error_sq <- 1
#
# n_11 <- 1
# n_12 <- 2
# n_21 <- 2
# n_22 <- 1
# n_23 <- 1
#
# n_dotdot <- 7
# c_dot <- 5

m_11 <- (n_11 * sig_b_sq) + error_sq
m_12 <- (n_12 * sig_b_sq) + error_sq
m_21 <- (n_21 * sig_b_sq) + error_sq
m_22 <- (n_22 * sig_b_sq) + error_sq
m_23 <- (n_23 * sig_b_sq) + error_sq

n_ij <- c(n_11, n_12, n_21, n_22, n_23)
m_ij <- c(m_11, m_12, m_21, m_22, m_23)

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
t_ee <- (A_102 - ((2 * sig_a_sq * A_113) / q_1) + ((sig_a_sq^2 * A_112^2) / q_1^2) + ((n_dotdot - c_dot)/error_sq^2)) +
  (A_202 - ((2 * sig_a_sq * A_213) / q_2) + ((sig_a_sq^2 * A_212^2) / q_2^2) + ((n_dotdot - c_dot)/error_sq^2))

elements = c(t_aa, t_ab, t_ae,
             t_ab, t_bb, t_be,
             t_ae, t_be, t_ee)

test <- 2 * solve(matrix(elements, 3, 3))
round(test, 3)
round(general_variance_3VC(9, c(5, 4), c(2, 3, 1, 2, 1), 1, 1, 1), 3)
# round(general_variance_3VC(90, c(50, 40), c(20, 30, 10, 20, 10), 1, 1, 1), 3)
# round(general_variance_3VC(7, c(3, 4), c(1, 2, 2, 1, 1), 1, 1, 1), 3)

test
general_variance_3VC(9, c(5, 4), c(2, 3, 1, 2, 1), 1, 1, 1)

# Searle 1970 and my function are close. All the terms with respect to error
# seem off by a factor of 2. All other elements are close.


