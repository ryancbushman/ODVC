#' generate_designs_u
#'
#' generate_designs_u allows users to generate a data.frame of all possible
#' one-way nested random effects designs with *N* observations and *a* number of
#' groups. The data.frame contains A and D scores and relative efficiencies
#' calculated as a function of the provided hypothesized value of
#' \eqn{\sigma^2_A}.
#'
#' @param N an integer representing the total number of design points
#' @param a an integer representing the total number of groups
#' @param sig_a_sq a vector of doubles representing the values of \eqn{\sigma^2_A}
#' to try
#' @param error_sq a double representing the value of \eqn{\sigma^2}
#'
#' @return a data.frame of experiment designs of size N
#' @export
#'
#' @examples
#' candidates <- generate_designs_U(N = 20, a = 4, sig_a_sq = 2, error_sq = 1)
generate_designs_U <- function(N, a, sig_a_sq, error_sq){
  if (N %% 1 != 0) {
    stop("N must be an integer")
  }
  if (a %% 1 != 0) {
    stop("The argument a must be an integer")
  }
  if (!is.numeric(sig_a_sq)) {
    stop("sig_a_sq must be a numeric value")
  }
  if (!is.numeric(error_sq)) {
    stop("error_sq must be a numeric value")
  }

  unbalanced_designs <- data.frame("N" = double(10000000),
                                   "a" = double(10000000),
                                   "n_i" = character(10000000),
                                   "sig_a_sq" = character(10000000),
                                   "A_Score" = double(10000000),
                                   "D_Score" = double(10000000))


  reps <- partitions::restrictedparts(n = N, m = a, include.zero = FALSE)
  count = 0

  for (i in seq_along(sig_a_sq)) {
    for (j in 1:ncol(reps)) {
      count = count + 1
      info <- general_variance_2VC(N = N, n = reps[, j], a = a, sig_a_sq = sig_a_sq[i], error_sq = 1)
      unbalanced_designs$N[count] = N
      unbalanced_designs$a[count] = a
      unbalanced_designs$n_i[count] = paste(as.character(reps[1:a, j]), collapse = " ")
      unbalanced_designs$sig_a_sq[count] = sig_a_sq[i]
      unbalanced_designs$A_Score[count] <- A_crit(info)
      unbalanced_designs$D_Score[count] <- D_crit(info)
    }
  }
  unbalanced_designs <- unbalanced_designs[unbalanced_designs$N != 0, ]

  unbalanced_designs$Relative.A.Efficiency <- numeric(length(unbalanced_designs$N))
  unbalanced_designs$Relative.D.Efficiency <- numeric(length(unbalanced_designs$N))
  for (i in unique(unbalanced_designs$sig_a_sq)) {
    temp <- unbalanced_designs[unbalanced_designs$sig_a_sq == i, ]

    OD_D <- temp[which.min(temp$D_Score), ]
    Relative.D.Efficiency <- 100 * OD_D$D_Score / temp$D_Score

    OD_A <- temp[which.min(temp$A_Score), ]
    Relative.A.Efficiency <- 100 * OD_A$A_Score / temp$A_Score

    unbalanced_designs[unbalanced_designs$sig_a_sq == i, ]$Relative.A.Efficiency <- Relative.A.Efficiency
    unbalanced_designs[unbalanced_designs$sig_a_sq == i, ]$Relative.D.Efficiency <- Relative.D.Efficiency
  }
  return(unbalanced_designs)
}
