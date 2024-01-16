#' generate_two_way_designs
#'
#' generate_two_way_designs allows users to specify the parameters of a
#' two-way nested random effects design and the class of designs they would
#' like to select from to generate a data.frame of candidate designs. The
#' classes are named C_(3,2) and C_(3,3) in which the first subscript refers to
#' the number of variance components in the model and the second subscript
#' refers to the max number of sub-groups per level of alpha and max number of
#' replicates per sub-group.
#'
#' @param N the total number of desired replicates in the experiment
#' @param s refers to desired class of experiments. An entry of 2 will produce
#' designs from class C_3_2 while an entry of 3 will produce designs from class
#' C_3_3
#' @param sig_a_sq a double representing the estimate of \eqn{\sigma^2_A}
#' @param sig_b_sq a double representing the estimate of \eqn{\sigma^2_B}
#' @param error_sq a double representing the estimate of \eqn{\sigma^2}
#'
#' @return a data.frame of all designs of size N from the selected class such
#' that all variance components can be estimated
#' @export
#'
#' @examples
#' candidates <- generate_two_way_designs(N = 12, s = 2, sig_a_sq = 1,
#'                                        sig_b_sq = 1, error_sq = 1)
#'
generate_two_way_designs <- function(N, s, sig_a_sq, sig_b_sq, error_sq) {
  if (N %% 1 != 0) {
    stop("N must be an integer")
  }
  if (!(s %in% c(2, 3))) {
    stop("s must be either the value 2 or 3")
  }
  if (!(is.numeric(sig_a_sq))) {
    stop("sig_a_sq must be a numeric value")
  }
  if (!(is.numeric(sig_b_sq))) {
    stop("sig_b_sq must be a numeric value")
  }
  if (!(is.numeric(error_sq))) {
    stop("error_sq must be a numeric value")
  }
  max_atoms <- N - 2
  if (s == 2) { # C_3,2 class has 5 atoms, see Delgado
    n_i_dot <- list(4, 3, 2, 2, 1) # number of total reps per level of alpha
    n_ij <- list(c(2, 2), c(2, 1), c(1, 1), 2, 1) # number of reps per level of beta nested in alpha

    ind <- gtools::combinations(n = 6, r = max_atoms, v = 0:5, repeats.allowed = TRUE) # all designs with up to max number of atoms
  } else if (s == 3) {
    n_i_dot <- list(9, 8, 7, 7, 6, 5, 6, 5, 4, 3, 6, 5, 4, 4, 3, 2, 3, 2, 1)
    n_ij <- list(c(3, 3, 3), c(3, 3, 2), c(3, 3, 1), c(3, 2, 2),
                 c(3, 2, 1), c(3, 1, 1), c(2, 2, 2), c(2, 2, 1), c(2, 1, 1),
                 c(1, 1, 1), c(3, 3), c(3, 2), c(3, 1), c(2, 2), c(2, 1), c(1, 1),
                 3, 2, 1)
    ind <- gtools::combinations(n = 20, r = max_atoms, v = 0:19, repeats.allowed = TRUE)
  }
  design_dots <- list()
  design_reps <- list()
  for (i in seq_len(nrow(ind))) { # apply indices to n_i_dot and n_ij to get designs
    design_dots[[i]] <- unlist(n_i_dot[ind[i, ]])
    design_reps[[i]] <- unlist(n_ij[ind[i, ]])
  }
  design_dots <- design_dots[-which(sapply(design_dots, sum) != N)]
  design_reps <- design_reps[-which(sapply(design_reps, sum) != N)]

  D_score <- numeric(length = length(design_dots))
  for (i in seq_len(length(design_dots))){
    if (det(info_3VC(N = N,
                     n_i_dot = design_dots[[i]],
                     n_ij = design_reps[[i]],
                     sig_a_sq = sig_a_sq,
                     sig_b_sq = sig_b_sq,
                     error_sq = error_sq)) > (.Machine$double.eps)^(0.5)) {
      D_score[i] <- D_crit(general_variance_3VC(N = N,
                                                n_i_dot = design_dots[[i]],
                                                n_ij = design_reps[[i]],
                                                sig_a_sq = sig_a_sq,
                                                sig_b_sq = sig_b_sq,
                                                error_sq = error_sq))

    }
  }
  final_indices <- which(D_score != 0)
  D_score <- D_score[D_score != 0]

  A_score <- numeric(length = length(design_dots))
  for (i in seq_len(length(design_dots))){
    if (det(info_3VC(N = N,
                     n_i_dot = design_dots[[i]],
                     n_ij = design_reps[[i]],
                     sig_a_sq = sig_a_sq,
                     sig_b_sq = sig_b_sq,
                     error_sq = error_sq)) > (.Machine$double.eps)^(0.5)) {
      A_score[i] <- A_crit(general_variance_3VC(N = N,
                                                n_i_dot = design_dots[[i]],
                                                n_ij = design_reps[[i]],
                                                sig_a_sq = sig_a_sq,
                                                sig_b_sq = sig_b_sq,
                                                error_sq = error_sq))

    }
  }
  A_score <- A_score[A_score != 0]

  Releff_A <- 100 * min(A_score) / A_score
  Releff_D <- 100 * min(D_score) / D_score

  designs <- data.frame("N" = rep(N, length(design_dots[final_indices])),
                        "N_i_dot" = as.character(design_dots[final_indices]),
                        "N_ij" = as.character(design_reps[final_indices]),
                        "D_score" = D_score,
                        "Relative D Efficiency" = Releff_D,
                        "A_score" = A_score,
                        "Relative A Efficiency" = Releff_A)

  return(designs)
}

info_3VC <- function(N, n_i_dot, n_ij, sig_a_sq, sig_b_sq, error_sq) {
  MLE_var <- matrix(rep(0, 9), nrow = 3, ncol = 3)
  J_a <- vector(mode = "list", length = length(n_i_dot))
  for (i in seq_along(n_i_dot)) {
    J_a[[i]] <- n_i_dot[i] * J_n(n_i_dot[i])
  }
  J_b <- vector(mode = "list", length = length(n_ij))
  for (j in seq_along(n_ij)) {
    J_b[[j]] <- n_ij[j] * J_n(n_ij[j])
  }
  derivatives <- list(do.call(statespacer::BlockMatrix, J_a),
                      do.call(statespacer::BlockMatrix, J_b), diag(1, N))
  temp_var <- V_3VC(N, n_i_dot, n_ij, sig_a_sq, sig_b_sq, error_sq)
  for (i in seq_along(derivatives)) {
    for (j in seq_along(derivatives)) {
      MLE_var[i, j] <- (1 / 2) * sum(diag(solve(temp_var) %*%
                                            derivatives[[j]] %*%
                                            solve(temp_var) %*%
                                            derivatives[[i]]))
    }
  }
  return(MLE_var)
}

generate_data <- function(N, s) {
  max_atoms <- N - 2
  if (s == 2) { # C_3,2 class has 5 atoms, see Delgado
    n_i_dot <- list(4, 3, 2, 2, 1) # number of total reps per level of alpha
    n_ij <- list(c(2, 2), c(2, 1), c(1, 1), 2, 1) # number of reps per level of beta nested in alpha

    ind <- gtools::combinations(n = 6, r = max_atoms, v = 0:5, repeats.allowed = TRUE) # all designs with up to max number of atoms
  } else if (s == 3) {
    n_i_dot <- list(9, 8, 7, 7, 6, 5, 6, 5, 4, 3, 6, 5, 4, 4, 3, 2, 3, 2, 1)
    n_ij <- list(c(3, 3, 3), c(3, 3, 2), c(3, 3, 1), c(3, 2, 2),
                 c(3, 2, 1), c(3, 1, 1), c(2, 2, 2), c(2, 2, 1), c(2, 1, 1),
                 c(1, 1, 1), c(3, 3), c(3, 2), c(3, 1), c(2, 2), c(2, 1), c(1, 1),
                 3, 2, 1)
    ind <- gtools::combinations(n = 20, r = max_atoms, v = 0:19, repeats.allowed = TRUE)
  }
  design_dots <- list()
  design_reps <- list()
  for (i in seq_len(nrow(ind))) { # apply indices to n_i_dot and n_ij to get designs
    design_dots[[i]] <- unlist(n_i_dot[ind[i, ]])
    design_reps[[i]] <- unlist(n_ij[ind[i, ]])
  }
  design_dots <- design_dots[-which(sapply(design_dots, sum) != N)]
  design_reps <- design_reps[-which(sapply(design_reps, sum) != N)]

  final_indices <- numeric()
  count = 0
  for (i in seq_along(design_dots)) {
    count = count + 1
    if (rcond(info_3VC(N = N,
                       n_i_dot = design_dots[[i]],
                       n_ij = design_reps[[i]],
                       sig_a_sq = 1,
                       sig_b_sq = 1,
                       error_sq = 1)) > (.Machine$double.eps)^(0.5)) {
      final_indices <- c(final_indices, count)
    }
  }
  #final_indices <- which(D_score != 0)
  designs <- data.frame("N" = rep(N, length(design_dots[final_indices])),
                        "N_i_dot" = as.character(design_dots[final_indices]),
                        "N_ij" = as.character(design_reps[final_indices]))
  return(designs)
}

generate_two_way_designs_90 <- function(N, s, sig_a_sq, sig_b_sq, error_sq) {
  max_atoms <- N - 2
  if (s == 2) { # C_3,2 class has 5 atoms, see Delgado
    n_i_dot <- list(4, 3, 2, 2, 1) # number of total reps per level of alpha
    n_ij <- list(c(2, 2), c(2, 1), c(1, 1), 2, 1) # number of reps per level of beta nested in alpha

    ind <- gtools::combinations(n = 6, r = max_atoms, v = 0:5, repeats.allowed = TRUE) # all designs with up to max number of atoms
  } else if (s == 3) {
    n_i_dot <- list(9, 8, 7, 7, 6, 5, 6, 5, 4, 3, 6, 5, 4, 4, 3, 2, 3, 2, 1)
    n_ij <- list(c(3, 3, 3), c(3, 3, 2), c(3, 3, 1), c(3, 2, 2),
                 c(3, 2, 1), c(3, 1, 1), c(2, 2, 2), c(2, 2, 1), c(2, 1, 1),
                 c(1, 1, 1), c(3, 3), c(3, 2), c(3, 1), c(2, 2), c(2, 1), c(1, 1),
                 3, 2, 1)
    ind <- gtools::combinations(n = 20, r = max_atoms, v = 0:19, repeats.allowed = TRUE)
  }
  design_dots <- list()
  design_reps <- list()
  for (i in seq_len(nrow(ind))) { # apply indices to n_i_dot and n_ij to get designs
    design_dots[[i]] <- unlist(n_i_dot[ind[i, ]])
    design_reps[[i]] <- unlist(n_ij[ind[i, ]])
  }
  design_dots <- design_dots[-which(sapply(design_dots, sum) != N)]
  design_reps <- design_reps[-which(sapply(design_reps, sum) != N)]

  A_score <- numeric(length = length(design_dots))
  D_score <- numeric(length = length(design_dots))
  for (i in seq_len(length(design_dots))){
    if (det(info_3VC(N = N,
                     n_i_dot = design_dots[[i]],
                     n_ij = design_reps[[i]],
                     sig_a_sq = sig_a_sq,
                     sig_b_sq = sig_b_sq,
                     error_sq = error_sq)) > (.Machine$double.eps)^(0.5)) {
      D_score[i] <- D_crit(general_variance_3VC(N = N,
                                                n_i_dot = design_dots[[i]],
                                                n_ij = design_reps[[i]],
                                                sig_a_sq = sig_a_sq,
                                                sig_b_sq = sig_b_sq,
                                                error_sq = error_sq))
      A_score[i] <- A_crit(general_variance_3VC(N = N,
                                                n_i_dot = design_dots[[i]],
                                                n_ij = design_reps[[i]],
                                                sig_a_sq = sig_a_sq,
                                                sig_b_sq = sig_b_sq,
                                                error_sq = error_sq))

    }
  }
  # final_indices <- which(D_score != 0)
  D_score <- D_score[D_score != 0]
  A_score <- A_score[A_score != 0]

  Releff_A <- 100 * min(A_score) / A_score
  Releff_D <- 100 * min(D_score) / D_score

  final_indices <- which(Releff_A >= 90 | Releff_D >= 90)

  designs <- data.frame("N" = rep(N, length(design_dots[final_indices])),
                        "N_i_dot" = as.character(design_dots[final_indices]),
                        "N_ij" = as.character(design_reps[final_indices]),
                        "D_score" = D_score[final_indices],
                        "Relative D Efficiency" = Releff_D[final_indices],
                        "A_score" = A_score[final_indices],
                        "Relative A Efficiency" = Releff_A[final_indices])

  return(designs)
}


