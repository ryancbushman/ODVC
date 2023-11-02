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
#' @param criteria a character value representing the desired design criteria used to calculate relative efficiency
#'
#' @return a data.frame object of balanced designs meeting user specifications
#' @export
#'
#' @examples
#'
#' candidate_designs <- generate_designs_B(ngroups = c(3, 5), nreps = c(5, 10),
#' taus = c(1, 2))
#' designs <- subset_designs_B(data = candidate_designs, N = 100, criteria = "D")

subset_designs_B <- function(data, N) {
  data <- data[which(data$N == N), ]
  data$releff_A <- numeric(length(data$N))
  data$releff_D <- numeric(length(data$N))
  for (i in unique(data$tau)) {
    temp <- data[data$tau == i, ]

    OD_D <- temp[which.min(temp$D_Score), ]
    releff_D <- 100 * OD_D$D_Score / temp$D_Score

    OD_A <- temp[which.min(temp$A_Score), ]
    releff_A <- 100 * OD_A$A_Score / temp$A_Score

    data[data$tau == i, ]$releff_A <- releff_A
    data[data$tau == i, ]$releff_D <- releff_D
  }

  # if (criteria == "D") {
  #   OD <- data[which.min(data$D_Score), ]
  #   data$releff <- 100 * OD$D_Score / data$D_Score
  # } else if (criteria == "A") {
  #   OD <- data[which.min(data$A_Score), ]
  #   data$releff <- 100 * OD$A_Score / data$A_Score
  # }


  return(data)
}
