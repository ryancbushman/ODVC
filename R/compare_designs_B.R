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
#' @param criteria a string value, either "D" or "A", to indicate which
#' optimality criteria to score designs on
#'
#' @return a dashboard of ggplot objects
#' @import ggplot2
#' @export
#'
#' @examples
#' candidate_designs <- generate_designs_B(ngroups = c(2, 3, 4, 6),
#' nreps = c(2, 3, 4, 6), taus = c(1, 2))
#' candidate_designs <- subset_designs_B(data = candidate_designs, N = 12)
#' compare_designs_B(designs = candidate_designs, criteria = "D")

compare_designs_B <- function(designs, criteria){
  if (!is.data.frame(designs)) {
    stop("designs must be a data.frame created using the subset_designs_B()
         function")
  }
  if (!(criteria %in% c("A", "D"))) {
    stop("criteria must be either the string A or D")
  }
  for (i in unique(designs$tau)) {
    designs$D_Score[designs$tau == i] = designs$D_Score[designs$tau == i] / sd(designs$D_Score[designs$tau == i])
    designs$A_Score[designs$tau == i] = designs$A_Score[designs$tau == i] / sd(designs$A_Score[designs$tau == i])
  }

  designs$tau <- with(designs, factor(tau,levels = (unique(tau))))

  if (criteria == "D") {
    c_plot <- ggplot(data = designs, aes(x = a, y = D_Score, fill = tau)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = paste0("Designs of size ", designs$N[1]),
           x = "Number of Groups",
           y = paste0("Standardized D Score"),
           color = "tau") +
      scale_x_continuous(breaks = scales::pretty_breaks())

    r_plot <- ggplot(data = designs, aes(x = a, y = Relative.D.Efficiency, fill = tau)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = paste0("Designs of size ", designs$N[1]),
           x = "Number of Groups",
           y = paste0("D Relative Efficiency"),
           color = "tau") +
      scale_x_continuous(breaks = scales::pretty_breaks())

    taus <- unique(designs$tau)
    relative.efficiency.tau <- numeric()
    OD_score <- numeric(length = length(unique(designs$tau)))
    for (j in seq_along(taus)) {
        check <- designs$D_Score[designs$tau == taus[j]]
        OD_score[j] <- min(check)
    }

    for (k in seq_along(taus)) {
        temp <- 100 * OD_score[k] / designs$D_Score[designs$tau == taus[k]]
        relative.efficiency.tau <- c(relative.efficiency.tau, temp)
    }

    designs <- designs[order(designs$tau), ]
    designs$relative.efficiency.tau <- relative.efficiency.tau

    tau_plots <- ggplot(data = designs, aes(x = a, y = relative.efficiency.tau)) +
          geom_bar(stat = 'identity') +
          labs(title = paste0("Relative Efficiency by values of Tau")) +
          ylab("Relative Efficiency") +
          xlab("Number of Groups") +
          scale_x_continuous(breaks = scales::pretty_breaks()) +
          theme(plot.title = element_text(size=12),
                axis.title = element_text(size = 12)) +
      facet_wrap(designs$tau, ncol = 2) +
      theme(plot.title = element_text(hjust = 0.5))

    grob <- vector(mode = 'list', length = 3)
    grob[[1]] <- c_plot
    grob[[2]] <- r_plot
    grob[[3]] <- tau_plots

    height = ceiling(length(taus) / 2)
    first_row <- c(1,1,2,2)
    rest <- matrix(rep(3, height * 4), nrow = height, ncol = 4)
    lay <- rbind(first_row, rest)
    gridExtra::grid.arrange(grobs = grob, layout_matrix = lay)
  } else {
    c_plot <- ggplot(data = designs, aes(x = a, y = A_Score, fill = tau)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = paste0("Designs of size ", designs$N[1]),
           x = "Number of Groups",
           y = paste0("Standardized A Score"),
           color = "tau") +
      scale_x_continuous(breaks = scales::pretty_breaks())

    r_plot <- ggplot(data = designs, aes(x = a, y = Relative.A.Efficiency, fill = tau)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = paste0("Designs of size ", designs$N[1]),
           x = "Number of Groups",
           y = paste0("A Relative Efficiency"),
           color = "tau") +
      scale_x_continuous(breaks = scales::pretty_breaks())

    taus <- unique(designs$tau)
    relative.efficiency.tau <- numeric()
    OD_score <- numeric(length = length(unique(designs$tau)))
    for (j in seq_along(taus)) {
      check <- designs$A_Score[designs$tau == taus[j]]
      OD_score[j] <- min(check)
    }

    for (k in seq_along(taus)) {
      temp <- 100 * OD_score[k] / designs$A_Score[designs$tau == taus[k]]
      relative.efficiency.tau <- c(relative.efficiency.tau, temp)
    }

    designs <- designs[order(designs$tau), ]
    designs$relative.efficiency.tau <- relative.efficiency.tau

    tau_plots <- ggplot(data = designs, aes(x = a, y = relative.efficiency.tau)) +
      geom_bar(stat = 'identity') +
      labs(title = paste0("Relative Efficiency by values of Tau")) +
      ylab("Relative Efficiency") +
      xlab("Number of Groups") +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      theme(plot.title = element_text(size=12),
            axis.title = element_text(size = 12)) +
      facet_wrap(designs$tau, ncol = 2) +
      theme(plot.title = element_text(hjust = 0.5))

    grob <- vector(mode = 'list', length = 3)
    grob[[1]] <- c_plot
    grob[[2]] <- r_plot
    grob[[3]] <- tau_plots

    height = ceiling(length(taus) / 2)
    first_row <- c(1,1,2,2)
    rest <- matrix(rep(3, height * 4), nrow = height, ncol = 4)
    lay <- rbind(first_row, rest)
    gridExtra::grid.arrange(grobs = grob, layout_matrix = lay)
  }
}



