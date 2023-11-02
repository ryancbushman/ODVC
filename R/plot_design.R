#' plot_design
#'
#' @param n a vector of integers representing the number of replications per group
#' @param a an integer representing the number of groups
#' @param sig_a_sq a double indicating the value of \deqn{\sigma^2_A}
#' @param error_sq a double indicating the value of \deqn{\sigma^2}
#' @param criteria a character "D" or "A" indicating the criteria to score on
#'
#' @return a tree diagram showing the structure of the experiment
#' @export
#'
#' @examples
#' plot_design(n = c(3,4,5), a = 3, sig_a_sq = 2, error_sq = 1, criteria = "D")
plot_design <- function(n, a, sig_a_sq, error_sq, criteria) {
  if (criteria == "D") {
    crit = D_crit
  } else {
    crit = A_crit
  }

  groups <- data.frame(from="origin", to=paste("group", seq(1, a), sep=""))
  reps_per_groups <- data.frame(from=rep(groups$to, n),
                                to=paste("rep", seq(1,sum(n)), sep="_"))
  edges <- rbind(groups, reps_per_groups)

  name <- unique(c(as.character(edges$from), as.character(edges$to)))
  vertices <- data.frame(
    name=name,
    group=c( rep(NA, a + 1), rep(paste("group", seq(1, a), sep=""), n))
  )

  mygraph <- graph_from_data_frame(edges, vertices=vertices)

  if (length(unique(n)) > 1) {
    title <- paste0("Unbalanced experiment with ", a, " groups")
    info <- one_way_cov_U(sum(n), a, n, sig_a_sq, error_sq)
    score <- crit(info)
  } else {
    title <- paste0("Balanced experiment with ", a,
                    " groups and ", n[1], " reps per group")
    info <- one_way_cov_B(error_sq = error_sq, tau = (sig_a_sq / error_sq), a = a, n = n[1])
    score <- crit(info)
  }

  ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
    geom_edge_elbow() +
    ggtitle(title) +
    geom_node_point(aes(filter=leaf, size = 2, color=group) , alpha=0.6) +
    theme_void() +
    theme(legend.position="none") +
    labs(caption = paste0(criteria, " Score: ", signif(score, 6))) +
    theme(plot.caption = element_text(size = 15),
          plot.title = element_text(size = 15))
}

