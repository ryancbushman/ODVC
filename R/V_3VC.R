#' V_3VC
#'
#' V_3VC returns the variance matrix for a 2-way nested ANOVA model that has
#' a total of three variance components: \deqn{\sigma^2_A}, \deqn{\sigma^2_B},
#' and \deqn{\sigma^2}
#'
#' @param N an integer representing the total number of experimental runs
#' @param n_i_dot integers representing the total number of reps in each group at
#' the first level of the experiment
#' @param n_ij integers representing the total number of reps in each group at
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
#' V_3VC(N = 8, n_i_dot = c(4, 4), n_ij = c(2, 2, 2, 2), sig_a_sq = 2, sig_b_sq = 3, error_sq = 1)
#'
#' # Unbalanced design
#' V_3VC(N = 9, n_i_dot = c(4, 5), n_ij = c(2, 3, 1, 2, 1), sig_a_sq = 2, sig_b_sq = 3, error_sq = 1)
V_3VC <- function(N, n_i_dot, n_ij, sig_a_sq, sig_b_sq, error_sq) {
  blocks_a <- vector(mode = "list", length = length(n_i_dot))
  for (i in seq_along(n_i_dot)) {
    blocks_a[[i]] <- n_i_dot[i] * J_n(n_i_dot[i]) # J_n defined in V_2VC.R
  }
  var_a <- do.call(BlockMatrix, blocks_a)
  blocks_b <- vector(mode = "list", length = length(n_ij))
  for (j in seq_along(n_ij)) {
    blocks_b[[j]] <- n_ij[j] * J_n(n_ij[j])
  }
  var_b <- do.call(BlockMatrix, blocks_b)
  var_e <- diag(1, N)
  var <- error_sq * var_e + sig_a_sq * var_a + sig_b_sq * var_b
  return(var)
}
