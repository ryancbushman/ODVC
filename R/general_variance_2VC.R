#' general_variance_2VC
#'
#' general_variance_2VC returns the MLE covariance matrix for optimization
#' in a nested ANOVA model with two variance components: \deqn{\sigma^2_A} and
#' \deqn{\sigma^2}
#'
#' @param N an integer representing the total number of experimental runs
#' @param n either a single integer or a vector of integers representing the
#' number of runs per group. If a single integer is used, then the experiment is
#' treated as a balanced design. If a vector is used, the user can specify
#' unbalanced replications for the groups
#' @param g an integer representing the number of groups
#' @param sig_a_sq a double representing the estimate of \deqn{\sigma^2_A}
#' @param error_sq a double representing the estimate of \deqn{\sigma^2}
#'
#' @return the covariance matrix of the model's maximum likelihood estimators
#' @export
#'
#' @examples
#' # Balanced design
#' general_variance_2VC(N = 8, n = 2, g = 4, sig_a_sq = 2, error_sq = 1)
#'
#' # Unbalanced design
#' general_variance_2VC(N = 8, n = c(2, 3, 3), g = 3, sig_a_sq = 2, error_sq = 1)
general_variance_2VC <- function(N, n, g, sig_a_sq, error_sq) {
  MLE_var <- matrix(rep(0, 4), nrow = 2, ncol = 2)
  if (length(n) == 1) {
    derivatives <- list(diag(1, N), kronecker(diag(1, g), matrix(rep(1, n^2), nrow = n, ncol = n)))
  } else {
    J_s <- vector(mode = "list", length = length(n))
    for (k in seq_along(n)) {
      J_s[[k]] <- n[k] * J_n(n[k])
    }
    derivatives <- list(diag(1, N), do.call(BlockMatrix, J_s))
  }
  temp_V <- V_2VC(n = n, g = g, sig_a_sq = sig_a_sq, error_sq = error_sq)
  for (i in seq_along(derivatives)) {
    for (j in seq_along(derivatives)) {
      MLE_var[i, j] <- (1 / 2) * sum(diag(solve(temp_V) %*% derivatives[[j]] %*% solve(temp_V) %*% derivatives[[i]]))
    }
  }
  return(solve(MLE_var))
}

