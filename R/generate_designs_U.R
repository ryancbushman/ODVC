#' Title
#'
#' @param N
#' @param a
#' @param vc
#' @param criteria
#' @param error
#'
#' @return
#' @export
#'
#' @examples
generate_designs_U <- function(N, a, vc, criteria, error){

  unbalanced_designs <- data.frame("N" = double(10000),
                                   "a" = double(10000),
                                   "n_i" = character(10000),
                                   "vc" = character(10000),
                                   "criteria" = character(10000),
                                   "score" = double(10000),
                                   "cross score" = double(10000))

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

  for (i in seq_along(vc)) {
    for (j in 1:ncol(reps)) {
      count = count + 1
      info <- one_way_cov_U(N = N, a = a, n_i = reps[, j], vc[i], error)
      unbalanced_designs$N[count] = N
      unbalanced_designs$a[count] = a
      unbalanced_designs$n_i[count] = paste(as.character(reps[1:a, j]), collapse = " ")
      unbalanced_designs$vc[count] = vc[i]
      unbalanced_designs$criteria[count] = criteria
      unbalanced_designs$score[count] = crit(info)
      unbalanced_designs$cross.score[count] = cross_crit(info)
    }
  }
  unbalanced_designs <- unbalanced_designs[unbalanced_designs$N != 0, ]
  return(unbalanced_designs)
}
