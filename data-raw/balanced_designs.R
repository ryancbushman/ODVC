# code to prepare `balanced_designs` dataset

balanced_designs <- data.frame("N" = double(96),
                               "a" = double(96),
                               "n" = character(96),
                               "tau" = character(96),
                               "Criteria" = double(96),
                                "Score" = double(96),
                               "Cross Score" = double(96))

ngroups <- c(5, 10, 20, 30)
nreps <- c(5, 10, 20)
taus <- c(0.5, 1, 2, 5)
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
      balanced_designs$Score[count] <- D_crit(one_way_balanced_MLE_covariance(
        error = 1,
        tau = taus[k],
        a = ngroups[i],
        n = nreps[j]))
      balanced_designs$Cross.Score[count] <- A_crit(one_way_balanced_MLE_covariance(
        error = 1,
        tau = taus[k],
        a = ngroups[i],
        n = nreps[j]))
    }
  }
}

for (i in seq_along(ngroups)) {
  for (j in seq_along(nreps)) {
    for (k in seq_along(taus)) {
      count = count + 1

      balanced_designs$N[count] <- ngroups[i] * nreps[j]
      balanced_designs$a[count] <- ngroups[i]
      balanced_designs$n[count] <- nreps[j]
      balanced_designs$tau[count] <- taus[k]
      balanced_designs$Criteria[count] <- "A"
      balanced_designs$Score[count] <- A_crit(one_way_balanced_MLE_covariance(
        error = 1,
        tau = taus[k],
        a = ngroups[i],
        n = nreps[j]))
      balanced_designs$Cross.Score[count] <- D_crit(one_way_balanced_MLE_covariance(
        error = 1,
        tau = taus[k],
        a = ngroups[i],
        n = nreps[j]))
    }
  }
}

usethis::use_data(balanced_designs, overwrite = TRUE)
