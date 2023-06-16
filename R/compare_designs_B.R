#' compare_designs_B
#'
#' compare_designs_B is used to create a dashboard of comparison plots for
#' designs of the same size N. The top left plot compares the score of the
#' designs across values of tau. The top right plot compares the relative
#' efficiency of all designs of size N despite the value of tau. The remaining
#' plots compare the relative efficiency of designs for specific values of tau.
#'
#' @param designs a data.frame object of designs of size N returned by the
#' function subset_designs_B()
#'
#' @return a dashboard of ggplot objects
#' @export
#'
#' @examples
#'
#' candidate_designs <- subset_designs_B(N = 100, criteria = "D")
#' compare_designs_B(designs = candidate_designs)
#'
compare_designs_B <- function(designs, criteria){
  c_plot <- ggplot(data = designs, aes(x = a, y = Score, fill = tau)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = paste0("Designs of size ", designs$N[1], " compared across number of groups and values of tau"),
         x = "Number of Groups",
         y = paste0(designs$Criteria[1], " Score"),
         color = "tau") +
    geom_text(aes(label = round(Score, 3)), size = 3,
              position = position_dodge(4.55), vjust = -0.5)

  r_plot <- ggplot(data = designs, aes(x = a, y = releff, fill = tau)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = paste0("Designs of size ", designs$N[1], " compared across number of groups and values of tau"),
         x = "Number of Groups",
         y = paste0(designs$Criteria[1], " Relative Efficiency"),
         color = "tau") +
    geom_text(aes(label = round(releff, 3)), size = 3,
              position = position_dodge(4.55), vjust = -0.5)

  subsets <- vector(mode = 'list', length = length(unique(designs$tau)))
  taus <- unique(designs$tau)

  for (i in seq_along(subsets)) {
    subsets[[i]] <- designs[designs$tau == taus[i], ]
  }

  OD_score <- numeric(length = length(unique(designs$tau)))
  releff_tau <- vector(mode = 'list', length = length(unique(designs$tau)))
  for (j in seq_along(taus)) {
    check <- designs$Score[designs$tau == taus[j]]
    OD_score[j] <- min(check)
  }

  for (k in seq_along(subsets)) {
    releff_tau[[k]] <- 100 * OD_score[k] / subsets[[k]]$Score
    subsets[[k]]$releff <- releff_tau[[k]]
  }

  tau_plots <- vector(mode = 'list', length = length(taus))
  for (l in seq_along(taus)) {
    tau_plots[[l]] <- ggplot(data = subsets[[l]], aes(x = a, y = releff)) +
      geom_bar(stat = 'identity') +
      labs(title = paste0("Relative Efficiency of Designs with Tau = ", taus[l]))
  }

  grob <- vector(mode = 'list', length = length(taus) + 1)
  grob[[1]] <- c_plot
  grob[[2]] <- r_plot
  for (m in seq_along(taus)) {
    grob[[m + 2]] <- tau_plots[[m]]
  }

  grid.arrange(grobs = grob, ncol = 2)

}



