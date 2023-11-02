#' generate_designs_3VC_B
#'
#' generate_designs_3VC_B generates a dataset of candidates experiment designs
#' for nested models with three variance components: \deqn{\sigma^2_A},
#' \deqn{\sigma^2_B}, and \deqn{\sigma^2}. Note that the input for arguments
#' g, n_i_dot, and n_ij, should be the same length. Additionally they should the index
#' for each input should correspond with each other.
#'
#' @param a a vector of integers that represent group sizes
#' @param n_i_dot a list of integer vectors that represent how many total replications belong to each of the groups at the A level
#' @param n_ij a list of integer vectors that represent how many replications belong to each of the groups at the B level
#' @param sig_a_sq a vector of doubles representing estimates of \deqn{\sigma^2_A}
#' @param sig_b_sq a vector of doubles representing estimates of \deqn{\sigma^2_B}
#' @param error_sq a vector of doubles representing estimates of \deqn{\sigma^2}
#'
#' @return a dataframe of candidate designs
#' @export
#'
#' @examples
#' n_i_dot = list(c(4, 4), c(4, 5))
#' g = c(2, 2)
#' n_ij = list(c(2,2,2,2), c(2,3,1,2,1))
#' sig_a_sq = 2
#' sig_b_sq = 3
#' error_sq = 1
#' test <- generate_designs_3VC_B(a = a, n_i_dot = n_i_dot, n_ij = n_ij, sig_a_sq = sig_a_sq, sig_b_sq = sig_b_sq, error_sq = error_sq)
#' test
generate_designs_3VC_B <- function(a, n_i_dot, n_ij, sig_a_sq, sig_b_sq, error_sq) {
  balanced_designs <- data.frame("N" = double(10000),
                                 "a" = double(10000),
                                 "n_i_dot" = character(10000),
                                 "n_ij" = character(10000),
                                 "sig_a_sq" = double(10000),
                                 "sig_b_sq" = double(10000),
                                 "error_sq" = double(10000),
                                 "A Score" = double(10000),
                                 "D Score" = double(10000),
                                 "relative efficiency A" = double(10000),
                                 "relative efficiency D" = double(10000))
  criteria <- c("A", "D")

  count = 0
  for (i in seq_along(g)){
    for (j in seq_along(sig_a_sq)) {
      for (k in seq_along(sig_b_sq)) {
        for (l in seq_along(error_sq)) {
          count = count + 1
          info <- general_variance_3VC(N = sum(n_ij[[i]]), n_i_dot = n_i_dot[[i]],
                                       n_ij = n_ij[[i]], sig_a_sq = sig_a_sq[j],
                                       sig_b_sq = sig_b_sq[k],
                                       error_sq = error_sq[l])
          balanced_designs$N[count] <- sum(n_ij[[i]])
          balanced_designs$g[count] <- g[i]
          balanced_designs$n_i_dot[count] <- paste(as.character(n_i_dot[[i]]), collapse = " ")
          balanced_designs$n_ij[count] <- paste(as.character(n_ij[[i]]), collapse = " ")
          balanced_designs$sig_a_sq[count] <- sig_a_sq[j]
          balanced_designs$sig_b_sq[count] <- sig_b_sq[k]
          balanced_designs$error_sq[count] <- error_sq[l]
          balanced_designs$A.Score[count] <- A_crit(info)
          balanced_designs$D.Score[count] <- D_crit(info)
        }
      }
    }
  }

  balanced_designs <- balanced_designs[balanced_designs$N != 0, ]
  balanced_designs$relative.efficiency.A = 100 * min(balanced_designs$A.Score) / balanced_designs$A.Score
  balanced_designs$relative.efficiency.D = 100 * min(balanced_designs$D.Score) / balanced_designs$D.Score
  return(balanced_designs)
}

n_i_dot = list(c(4, 4), c(4, 5))
g = c(2, 2)
n_ij = list(c(2,2,2,2), c(2,3,1,2,1))
sig_a_sq = 2
sig_b_sq = 3
error_sq = 1
test <- generate_designs_3VC_B(a = a, n_i_dot = n_i_dot, n_ij = n_ij, sig_a_sq = sig_a_sq, sig_b_sq = sig_b_sq, error_sq = error_sq)
test

A_crit(general_variance_3VC(8, c(4,4), c(2,2,2,2), 2, 3, 1))
A_crit(general_variance_3VC(9, c(4,5), c(2,3,1,2,1), 2, 3, 1))
D_crit(general_variance_3VC(8, c(4,4), c(2,2,2,2), 2, 3, 1))
D_crit(general_variance_3VC(9, c(4,5), c(2,3,1,2,1), 2, 3, 1))

# I can only have one function for the 3vc 2vc case if instead of partitions, I allow the user to specify
# how they would allocate reps.

n_i_dot = list(c(4, 4), c(4, 5))
g = c(2, 2)
n_ij = list(c(2,2,2,2), c(2,3,1,2,1))
sig_a_sq = 2
sig_b_sq = 3
error_sq = 1
test <- generate_designs_3VC_B(a = a, n_i_dot = n_i_dot, n_ij = n_ij, sig_a_sq = sig_a_sq, sig_b_sq = sig_b_sq, error_sq = error_sq)
test
