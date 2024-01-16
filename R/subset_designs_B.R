#' subset_designs_B
#'
#' subset_designs_B returns a dataset of balanced designs that have a user
#' specified number of design points. This dataset allows the user to compare
#' various designs of the same size across different numbers of groups,
#' replications, and ratio of variance components (tau)
#'
#' @param data a dataframe of designs generated using generate_designs_B()
#' @param N an integer value representing the total number of experiment design
#' points
#'
#' @return a data.frame object of balanced designs meeting user specifications
#' @export
#'
#' @examples
#'
#' candidate_designs <- generate_designs_B(ngroups = c(3, 5), nreps = c(5, 10),
#' taus = c(1, 2))
#' designs <- subset_designs_B(data = candidate_designs, N = 50)

subset_designs_B <- function(data, N) {
  if (!(is.data.frame(data))) {
    stop("data must be a dataframe generated from generate_designs_B()")
  }
  if (N %% 1 != 0) {
    stop("N must be an integer")
  }
  if (!(N %in% data$N)) {
    stop("The value of N must be represented in the N column of the data")
  }
  data <- data[which(data$N == N), ]
  data$Relative.A.Efficiency <- numeric(length(data$N))
  data$Relative.D.Efficiency <- numeric(length(data$N))
  for (i in unique(data$tau)) {
    temp <- data[data$tau == i, ]

    OD_D <- temp[which.min(temp$D_Score), ]
    Relative.D.Efficiency <- 100 * OD_D$D_Score / temp$D_Score

    OD_A <- temp[which.min(temp$A_Score), ]
    Relative.A.Efficiency <- 100 * OD_A$A_Score / temp$A_Score

    data[data$tau == i, ]$Relative.A.Efficiency <- Relative.A.Efficiency
    data[data$tau == i, ]$Relative.D.Efficiency <- Relative.D.Efficiency
  }
  return(data)
}
