#' generate_designs_B
#'
#' generate_designs_B creates a dataframe of experiment designs with user
#' specified parameters.
#'
#' @param ngroups a vector of integers specifying the number of groups
#' @param nreps a vector of integers specifying the number of replications per
#' group
#' @param taus a vector of doubles specifying the ratio of the variance components
#'
#' @return a dataframe of potential designs
#' @export
#'
#' @examples
#'
#' candidate_designs <- generate_designs_B(ngroups = c(3, 5), nreps = c(5, 10),
#' taus = c(1, 2))
#'
generate_designs_B <- function(ngroups = c(5, 10), nreps = c(5, 10),
                               taus = c(1, 2)){

  if (!(all(ngroups == floor(ngroups)))) {
    stop("All values in ngroups vector must be integers")
  }
  if (!(all(nreps == floor(nreps)))) {
    stop("All values in nreps vector must be integers")
  }
  if (!is.numeric(taus)) {
    stop("taus must be a vector of numerics")
  }

  iterations <- length(ngroups) * length(nreps) * length(taus)
  balanced_designs <- data.frame("N" = double(iterations),
                                 "a" = double(iterations),
                                 "n" = character(iterations),
                                 "tau" = character(iterations),
                                 "A_Score" = double(iterations),
                                 "D_Score" = double(iterations),
                                 "Relative.A.Efficiency" = double(iterations),
                                 "Relative.D.Efficiency" = double(iterations))

  count = 0

  for (i in seq_along(ngroups)) {
    for (j in seq_along(nreps)) {
      for (k in seq_along(taus)) {
        count = count + 1

        balanced_designs$N[count] <- ngroups[i] * nreps[j]
        balanced_designs$a[count] <- ngroups[i]
        balanced_designs$n[count] <- nreps[j]
        balanced_designs$tau[count] <- taus[k]
        balanced_designs$A_Score[count] <- A_crit(general_variance_2VC(
          N = ngroups[i] * nreps[j],
          n = nreps[j],
          a = ngroups[i],
          sig_a_sq = taus[k],
          error_sq = 1))
        balanced_designs$D_Score[count] <- D_crit(general_variance_2VC(
          N = ngroups[i] * nreps[j],
          n = nreps[j],
          a = ngroups[i],
          sig_a_sq = taus[k],
          error_sq = 1))
      }
    }
  }
  balanced_designs$Relative.A.Efficiency <- 100 * min(balanced_designs$A_Score) / balanced_designs$A_Score
  balanced_designs$Relative.D.Efficiency <- 100 * min(balanced_designs$D_Score) / balanced_designs$D_Score
  return(balanced_designs)
}
