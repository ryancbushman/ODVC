#' compare_designs_U
#'
#' compare_designs_U creates a scatterplot of dataset index and relative
#' efficiency for a dataset of unbalanced designs created from
#' generate_designs_U(). A reference line is included to easily identify designs
#' that are at least 90% efficient compared to the optimal design
#'
#' @param data a data.frame created using generate_designs_U()
#' @param criteria a character "D" or "A" to indicate the criteria designs are
#' scored on. Should match criteria used in the generate_designs_U() dataset
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' candidates <- generate_designs_U(N = 20, a = 5, sig_a_sq = 2, criteria = "D", error = 1)
#' compare_designs_U(data = candidates, criteria = "D")
compare_designs_U <- function(data, criteria) {
  if (criteria == "D") {
    relative.efficiency = data$Relative.D.Efficiency
  } else {
    relative.efficiency = data$Relative.A.Efficiency
  }
  ggplot(data = data, aes(y = relative.efficiency,
                          x = seq_along(relative.efficiency))) +
    geom_point(size = 4) +
    geom_text(aes(label = seq_along(relative.efficiency)), vjust = -0.5, size = 4) +
    xlab("Dataset Index") +
    ylab(paste0(criteria, " Relative Efficiency")) +
    ggtitle(paste0("Comparing Relative Efficiency Across Unbalanced Designs of size ", data$N[1])) +
    geom_abline(slope = 0, intercept = 90, color = "red") +
    scale_x_continuous(breaks = pretty_breaks())
}

