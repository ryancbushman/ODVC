generate_designs_2VC_B <- function(N, n, g, sig_a_sq, error_sq, criteria) {
  balanced_designs <- data.frame("N" = double(10000),
                                 "a" = double(10000),
                                 "n" = character(10000),
                                 "tau" = character(10000),
                                 "Criteria" = double(10000),
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
  count = 0

  for (i in seq_along(g)) {
    for (j in seq_along(n)) {
      for (k in seq_along(sig_a_sq)) {
        count = count + 1
        if (length(n[[j]]) == 1) {
          balanced_designs$N[count] <- g[i] * n[[j]]
        } else {
          balanced_designs$N[count] <- sum(n[[j]])
        }
        balanced_designs$a[count] <- g[i]
        balanced_designs$n[count] <- paste(as.character(n[[j]]), collapse = " ")
        balanced_designs$tau[count] <- sig_a_sq[k]
        balanced_designs$criteria[count] <- "D"
        balanced_designs$score[count] <- crit(general_variance_2VC(
          N = N,
          n = n[[j]],
          g = g[i],
          sig_a_sq = sig_a_sq[k],
          error_sq = error_sq))
        balanced_designs$cross.score[count] <- cross_crit(general_variance_2VC(
          N = N,
          n = n[[j]],
          g = g[i],
          sig_a_sq = sig_a_sq[k],
          error_sq = error_sq))
      }
    }
  }
  balanced_designs <- balanced_designs[balanced_designs$N != 0, ]
  balanced_designs$relative.efficiency = 100 *  min(balanced_designs$score) / balanced_designs$score
  return(balanced_designs)
}
