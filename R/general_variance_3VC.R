#' general_variance_3VC
#'
#' general_variance_3VC returns the MLE covariance matrix for optimization
#' in a nested ANOVA model with three variance components: \deqn{\sigma^2_A},
#' \deqn{\sigma^2_B}, and \deqn{\sigma^2}
#'
#' @param N an integer representing the total number of experimental runs
#' @param n_a a vector of integers representing the total number of runs
#' at the A level of the model. The elements of the vector should sum to N
#' @param n_b a vector of integers representing the number of runs per group
#' at the B level of the model. The elements of the vector should sum to N
#' @param sig_a_sq a double representing the estimate of \deqn{\sigma^2_A}
#' @param sig_b_sq a double representing the estimate of \deqn{\sigma^2_B}
#' @param error_sq a double representing the estimate of \deqn{\sigma^2}
#'
#' @return the covariance matrix of the model's maximum likelihood estimators
#' @export
#'
#' @examples
#' # Balanced design
#' general_variance_3VC(N = 8, n_a = c(4, 4), n_b = c(2, 2, 2, 2),
#' sig_a_sq = 0.5, sig_b_sq = 2, error_sq = 1)
#'
#' # Unbalanced design
#' general_variance_3VC(N = 9, n_a = c(4, 5), n_b = c(2, 3, 1, 2, 1),
#' sig_a_sq = 0.5, sig_b_sq = 2, error_sq = 1)
general_variance_3VC <- function(N, n_a, n_b, sig_a_sq, sig_b_sq, error_sq) {
  MLE_var <- matrix(rep(0, 9), nrow = 3, ncol = 3)
  J_a <- vector(mode = "list", length = length(n_a))
  for (i in seq_along(n_a)) {
    J_a[[i]] <- n_a[i] * J_n(n_a[i])
  }
  J_b <- vector(mode = "list", length = length(n_b))
  for (j in seq_along(n_b)) {
    J_b[[j]] <- n_b[j] * J_n(n_b[j])
  }
  derivatives <- list(diag(1, N), do.call(BlockMatrix, J_a), do.call(BlockMatrix, J_b))
  temp_var <- V_3VC(N, n_a, n_b, sig_a_sq, sig_b_sq, error_sq)
  for (i in seq_along(derivatives)) {
    for (j in seq_along(derivatives)) {
      MLE_var[i, j] <- (1 / 2) * sum(diag(solve(temp_var) %*% derivatives[[j]] %*% solve(temp_var) %*% derivatives[[i]]))
    }
  }
  return(solve(MLE_var))
}
