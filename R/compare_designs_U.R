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
#' @param top_5 a logical argument. If set to TRUE the output plot will assign
#' data labels to only the top 5 performing designs. If set to FALSE, the output
#' plot will assign data labels to all designs.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' candidates <- generate_designs_U(N = 20, a = 5, sig_a_sq = 2, error = 1)
#' compare_designs_U(data = candidates, criteria = "D", top_5 = TRUE)

compare_designs_U <- function(data, criteria, top_5 = FALSE) {
  if (!is.data.frame(data)) {
    stop("data must be a dataframe of candidate designs generated using
         generate_designs_U(), generate_two_way_designs(), or
         generate_designs_B()")
  }
  if (!(criteria %in% c("A", "D"))) {
    stop("criteria must be either the string A or D")
  }
  if (criteria == "D") {
    relative.efficiency = data$Relative.D.Efficiency
  } else if (criteria == "A") {
    relative.efficiency = data$Relative.A.Efficiency
  }
  data$id <- seq_len(nrow(data))

  if (top_5 == FALSE) {
    ggplot(data = data, aes(y = relative.efficiency,
                            x = id)) +
      geom_point(size = 1) +
      geom_text(aes(label = id), vjust = -0.5, size = 4) +
      xlab("Dataset Index") +
      ylab(paste0(criteria, " Relative Efficiency")) +
      ggtitle(paste0("Comparing Relative Efficiency Across Unbalanced Designs of size ", data$N[1])) +
      geom_abline(slope = 0, intercept = 90, color = "red") +
      scale_x_continuous(breaks = scales::pretty_breaks())
  } else {
    label_data <- head(data[order(relative.efficiency, decreasing = TRUE), ], 5)
    if (criteria == "D") {
      l_relative.efficiency = label_data$Relative.D.Efficiency
    } else if (criteria == "A") {
      l_relative.efficiency = label_data$Relative.A.Efficiency
    }


    ggplot(data = data, aes(y = relative.efficiency, x = seq_along(relative.efficiency))) +
      geom_point(size = 1) +
      geom_text(data = label_data,
                aes(label = id, x = id, y = l_relative.efficiency, vjust = -0.5), size = 4) +
      xlab("Dataset Index") +
      ylab(paste0(criteria, " Relative Efficiency")) +
      ggtitle(paste0("Comparing Relative Efficiency Across Unbalanced Designs of size ", data$N[1])) +
      geom_abline(slope = 0, intercept = 90, color = "red") +
      scale_x_continuous(breaks = scales::pretty_breaks())
  }
}

