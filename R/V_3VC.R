#' V_3VC
#'
#' V_3VC returns the variance matrix for a 2-way nested ANOVA model that has
#' a total of three variance components: \deqn{\sigma^2_A}, \deqn{\sigma^2_B},
#' and \deqn{\sigma^2}
#'
#' @param N an integer representing the total number of experimental runs
#' @param n_a integers representing the total number of reps in each group at
#' the first level of the experiment
#' @param n_b integers representing the total number of reps in each group at
#' the second level of the experiment
#' @param sig_a_sq a double representing the estimate of \deqn{\sigma^2_A}
#' @param sig_b_sq a double representing the estimate of \deqn{\sigma^2_B}
#' @param error_sq a double representing the estimate of \deqn{\sigma^2}
#'
#' @return the variance matrix of a 2-way nested anova model
#' @export
#'
#' @examples
#' # Balanced design
#' V_3VC(N = 8, n_a = c(4, 4), n_b = c(2, 2, 2, 2), sig_a_sq = 2, sig_b_sq = 3, error_sq = 1)
#'
#' # Unbalanced design
#' V_3VC(N = 9, n_a = c(4, 5), n_b = c(2, 3, 1, 2, 1), sig_a_sq = 2, sig_b_sq = 3, error_sq = 1)
V_3VC <- function(N, n_a, n_b, sig_a_sq, sig_b_sq, error_sq) {
  blocks_a <- vector(mode = "list", length = length(n_a))
  for (i in seq_along(n_a)) {
    blocks_a[[i]] <- n_a[i] * J_n(n_a[i])
  }
  var_a <- do.call(BlockMatrix, blocks_a)
  blocks_b <- vector(mode = "list", length = length(n_b))
  for (j in seq_along(n_b)) {
    blocks_b[[j]] <- n_b[j] * J_n(n_b[j])
  }
  var_b <- do.call(BlockMatrix, blocks_b)
  var_e <- diag(1, N)
  var <- error_sq * var_e + sig_a_sq * var_a + sig_b_sq * var_b
  return(var)
}
