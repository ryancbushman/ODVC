#' plot_design
#'
#' @param n a vector of integers representing the number of replications per group
#' @param g an integer representing the number of groups
#' @param sig_a a double indicating the value of \deqn{\sigma^2_A}
#' @param error a double indicating the value of \deqn{\sigma^2}
#' @param criteria a character "D" or "A" indicating the criteria to score on
#'
#' @return a tree diagram showing the structure of the experiment
#' @export
#'
#' @examples
#' plot_design(n = c(3,4,5), g = 3, sig_a = 2, error = 1, criteria = "D")
plot_design <- function(n, g, sig_a, error, criteria) {
  if (criteria == "D") {
    crit = D_crit
  } else {
    crit = A_crit
  }

  groups <- data.frame(from="origin", to=paste("group", seq(1, g), sep=""))
  reps_per_groups <- data.frame(from=rep(groups$to, n),
                                to=paste("rep", seq(1,sum(n)), sep="_"))
  edges <- rbind(groups, reps_per_groups)

  name <- unique(c(as.character(edges$from), as.character(edges$to)))
  vertices <- data.frame(
    name=name,
    group=c( rep(NA, g + 1) , rep(paste("group", seq(1, g), sep=""), n))
  )

  mygraph <- graph_from_data_frame( edges, vertices=vertices)

  if (length(unique(n)) > 1) {
    title <- paste0("Unbalanced experiment with ", g, " groups")
    info <- one_way_cov_U(sum(n), g, n, sig_a, error)
    score <- crit(info)
  } else {
    title <- paste0("Balanced experiment with ", g,
                    " groups and ", n[1], " reps per group")
    info <- one_way_cov_B(error = error, tau = (sig_a / error), a = g, n = n[1])
    score <- crit(info)
  }

  ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
    geom_edge_elbow() +
    ggtitle(title) +
    labs(caption = paste0(criteria, " Score: ", signif(score, 6))) +
    theme_void()
}
