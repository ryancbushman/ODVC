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
#' @param criteria a character value representing the desired design criteria
#'
#' @return a data.frame object of balanced designs meeting user specifications
#' @export
#'
#' @examples
#'
#' designs <- subset_designs_B(N = 100, criteria = "D")

subset_designs_B <- function(data, N, criteria) {
  data <- data[which(data$N == N), ]

  if (criteria == "D") {
    OD <- data[which.min(data$Score[data$Criteria == "D"]), ]
  } else if (criteria == "A") {
    OD <- data[which.min(data$Score[data$Criteria == "A"]), ]
  }

  data$releff[data$Criteria == criteria] <- 100 * OD$Score / data$Score[data$Criteria == criteria]
  return(data)
}
