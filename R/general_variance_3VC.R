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
  if (N %% 1 != 0) {
    stop("N must be an integer")
  }
  if (sum(n_i_dot) != N) {
    stop("The sum of n_i_dot must be equal to N")
  }
  if (!(all(n_i_dot == floor(n_i_dot)))) {
    stop("All entries of n_i_dot must be integers")
  }
  if (sum(n_ij) != N) {
    stop("The sum of n_ij must be equal to N")
  }
  if (!is.numeric(sig_a_sq)) {
    stop("sig_a_sq must be a numeric value")
  }
  if (!is.numeric(sig_b_sq)) {
    stop("sig_a_sq must be a numeric value")
  }
  if (!is.numeric(error_sq)) {
    stop("error_sq must be a numeric value")
  }
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
