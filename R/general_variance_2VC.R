#' general_variance_2VC
#'
#' general_variance_2VC returns the MLE covariance matrix for optimization
#' in a nested ANOVA model with two variance components: \eqn{\sigma^2_A} and
#' \eqn{\sigma^2}
#'
#' @param N an integer representing the total number of experimental runs
#' @param n either a single integer or a vector of integers representing the
#' number of runs per group. If a single integer is used, then the experiment is
#' treated as a balanced design. If a vector is used, the user can specify
#' unbalanced replications for the groups
#' @param a an integer representing the number of groups
#' @param sig_a_sq a double representing the estimate of \eqn{\sigma^2_A}
#' @param error_sq a double representing the estimate of \eqn{\sigma^2}
#'
#' @return the covariance matrix of the model's maximum likelihood estimators
#' @export
#'
#' @examples
#' # Balanced design
#' general_variance_2VC(N = 8, n = 2, a = 4, sig_a_sq = 2, error_sq = 1)
#'
#' # Unbalanced design
#' general_variance_2VC(N = 8, n = c(2, 3, 3), a = 3, sig_a_sq = 2, error_sq = 1)
general_variance_2VC <- function(N, n, a, sig_a_sq, error_sq) {
  if (N %% 1 != 0) {
    stop("N must be an integer")
  }
  if (!(all(n == floor(n)))) {
    stop("All entries of n must be integers")
  }
  if (a %% 1 != 0) {
    stop("The argument a must be an integer")
  }
  if (length(n) == 1) {
    if (n * a != N) {
      stop("a single value for the argument n suggests that n * a must equal N")
    }
  }
  if (length(n) != 1) {
    if (sum(n) != N) {
      stop("The values in the argument n must sum to equal N")
    }
  }
  if (!is.numeric(sig_a_sq)) {
    stop("sig_a_sq must be a numeric value")
  }
  if (!is.numeric(error_sq)) {
    stop("error_sq must be a numeric value")
  }
  MLE_var <- matrix(rep(0, 4), nrow = 2, ncol = 2)
  if (length(n) == 1) {
    derivatives <- list(diag(1, N), kronecker(diag(1, a), matrix(rep(1, n^2), nrow = n, ncol = n)))
  } else {
    J_s <- vector(mode = "list", length = length(n))
    for (k in seq_along(n)) {
      J_s[[k]] <- n[k] * J_n(n[k])
    }
    derivatives <- list(diag(1, N), do.call(statespacer::BlockMatrix, J_s))
  }
  temp_V <- V_2VC(n = n, a = a, sig_a_sq = sig_a_sq, error_sq = error_sq)
  for (i in seq_along(derivatives)) {
    for (j in seq_along(derivatives)) {
      MLE_var[i, j] <- (1 / 2) * sum(diag(solve(temp_V) %*% derivatives[[j]] %*% solve(temp_V) %*% derivatives[[i]]))
    }
  }
  return(solve(MLE_var))
}

#' V_2VC
#'
#' V_2VC returns the variance matrix for a one-way nested ANOVA model that has
#' a two variance components: \eqn{\sigma^2_A} and \eqn{\sigma^2}.
#'
#' @param n either a single integer or a vector of integers representing the
#' number of runs per group. If a single integer is used, then the experiment is
#' treated as a balanced design. If a vector is used, the user can specify
#' unbalanced replications for the groups
#' @param a an integer representing the number of groups
#' @param sig_a_sq a double representing the estimate of \eqn{\sigma^2_A}
#' @param error_sq a double representing the estimate of \eqn{\sigma^2}
#'
#' @return the variance matrix of a 2-way nested anova model
#' @export
#'
#' @examples
#' # Balanced design
#' V_2VC(n = 4, a = 2, sig_a_sq = 2, error_sq = 1)
#'
#' # Unbalanced design
#' V_2VC(n = c(3, 5), a = 2, sig_a_sq = 2, error_sq = 1)
V_2VC <- function(n, a, sig_a_sq, error_sq) {
  if (length(n) == 1) {
    first <- diag(1, nrow = a)
    second <- (n * sig_a_sq) * J_n(n)
    third <- error_sq * diag(1, nrow = n)
    var <- kronecker(first, (second + third))
  } else {
    J_s <- vector(mode = "list", length = length(n))
    for (i in seq_along(n)) {
      J_s[[i]] <- n[i] * J_n(n[i])
    }
    var <- error_sq * diag(1, sum(n)) + sig_a_sq * do.call(statespacer::BlockMatrix, J_s)
  }
  return(var)
}

J_n <- function(n) {
  (1 / n) * matrix(rep(1, n * n), nrow = n, ncol = n)
}

