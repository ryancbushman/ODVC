#' V_2VC
#'
#' V_2VC returns the variance matrix for a one-way nested ANOVA model that has
#' a two variance components: \deqn{\sigma^2_A} and \deqn{\sigma^2}.
#'
#' @param n either a single integer or a vector of integers representing the
#' number of runs per group. If a single integer is used, then the experiment is
#' treated as a balanced design. If a vector is used, the user can specify
#' unbalanced replications for the groups
#' @param a an integer representing the number of groups
#' @param sig_a_sq a double representing the estimate of \deqn{\sigma^2_A}
#' @param error_sq a double representing the estimate of \deqn{\sigma^2}
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
    var <- error_sq * diag(1, sum(n)) + sig_a_sq * do.call(BlockMatrix, J_s)
  }
  return(var)
}

J_n <- function(n) {
  (1 / n) * matrix(rep(1, n * n), nrow = n, ncol = n)
}
