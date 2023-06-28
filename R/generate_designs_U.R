#' Title
#'
#' @param N an integer representing the total number of design points
#' @param a an integer representing the total number of groups
#' @param sig_a a vector of doubles representing the values of \deqn{\sigma^2_A}
#' to try
#' @param criteria a character "D" or "A" to indicate the design criteria to
#' score on
#' @param error a double representing the value of \deqn{\sigma^2}
#'
#' @return a dataframe of experiment designs of size N
#' @export
#'
#' @examples
#' candidates <- generate_designs_U(N = 20, a = 4, sig_a = 2, criteria = "D",
#' error = 1)
generate_designs_U <- function(N, a, sig_a, criteria, error){

  unbalanced_designs <- data.frame("N" = double(10000),
                                   "a" = double(10000),
                                   "n_i" = character(10000),
                                   "sig_a" = character(10000),
                                   "criteria" = character(10000),
                                   "score" = double(10000),
                                   "cross score" = double(10000),
                                   "relative efficiency" = double(10000))

  if (criteria == "D") {
    crit = D_crit
  } else {
    crit = A_crit
  }

  if (criteria == "D") {
    cross_crit = A_crit
  } else {
    cross_crit = D_crit
  }

  reps <- getCols(parts(N), A = a, N = N)
  reps <- reps[1:a, ]
  count = 0

  for (i in seq_along(sig_a)) {
    for (j in 1:ncol(reps)) {
      count = count + 1
      info <- one_way_cov_U(N = N, a = a, n_i = reps[, j], sig_a[i], error)
      unbalanced_designs$N[count] = N
      unbalanced_designs$a[count] = a
      unbalanced_designs$n_i[count] = paste(as.character(reps[1:a, j]), collapse = " ")
      unbalanced_designs$sig_a[count] = sig_a[i]
      unbalanced_designs$criteria[count] = criteria
      unbalanced_designs$score[count] = crit(info)
      unbalanced_designs$cross.score[count] = cross_crit(info)
    }
  }
  unbalanced_designs <- unbalanced_designs[unbalanced_designs$N != 0, ]
  unbalanced_designs$relative.efficiency = 100 *  min(unbalanced_designs$score) / unbalanced_designs$score
  return(unbalanced_designs)
}
