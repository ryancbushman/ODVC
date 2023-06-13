#' compare_designs_B
#'
#' compare_designs_B is used to create a plot of balanced designs all with the
#' same number of design points, but varying number of groups and values of tau.
#'
#' @param designs a data.frame object returned by the function
#' subset_designs_B()
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' candidate_designs <- subset_designs_B(N = 100, criteria = "D")
#' compare_designs_B(designs = candidate_designs)
#'
compare_designs_B <- function(designs){
  ggplot(data = designs, aes(x = a, y = Score, fill = tau)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = paste0("Designs of size ", designs$N[1], " compared across number of groups and values of tau"),
         x = "Number of Groups",
         y = paste0(designs$Criteria[1], " Score"),
         color = "tau") +
    geom_text(aes(label = round(Score, 3)), size = 3,
              position = position_dodge(4.55), vjust = -0.5)
}



