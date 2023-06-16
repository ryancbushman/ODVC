#' generate_designs_B
#'
#' generate_designs_B creates a dataframe of experiment designs with user
#' specified parameters.
#'
#' @param ngroups a vector of integers specifying the number of groups
#' @param nreps a vector of integers specifying the number of replications per
#' group
#' @param taus a vector of doubles specifying the ratio of the variance components
#' @param criteria a character "D" or "A" specifying the criteria measure
#'
#' @return a dataframe of potential designs
#' @export
#'
#' @examples
#'
#' candidate_designs <- generate_designs_B(ngroups = c(3, 5), nreps = c(5, 10),
#' taus = c(1, 2), criteria = "D")
#'
generate_designs_B <- function(ngroups = c(5, 10), nreps = c(5, 10),
                               taus = c(1, 2), criteria = "D"){

  iterations <- length(ngroups) * length(nreps) * length(taus)
  balanced_designs <- data.frame("N" = double(iterations),
                                 "a" = double(iterations),
                                 "n" = character(iterations),
                                 "tau" = character(iterations),
                                 "Criteria" = double(iterations),
                                 "Score" = double(iterations),
                                 "Cross Score" = double(iterations))

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
  count = 0

  for (i in seq_along(ngroups)) {
    for (j in seq_along(nreps)) {
      for (k in seq_along(taus)) {
        count = count + 1

        balanced_designs$N[count] <- ngroups[i] * nreps[j]
        balanced_designs$a[count] <- ngroups[i]
        balanced_designs$n[count] <- nreps[j]
        balanced_designs$tau[count] <- taus[k]
        balanced_designs$Criteria[count] <- "D"
        balanced_designs$Score[count] <- crit(one_way_cov_B(
          error = 1,
          tau = taus[k],
          a = ngroups[i],
          n = nreps[j]))
        balanced_designs$Cross.Score[count] <- cross_crit(one_way_cov_B(
          error = 1,
          tau = taus[k],
          a = ngroups[i],
          n = nreps[j]))
      }
    }
  }
  return(balanced_designs)
}
