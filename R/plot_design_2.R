#' plot_design_2
#'
#' plot_design_2 plots a dendrogram representation of the design of a user
#' specified experiment.
#'
#' @param g an integer representing the number of groups
#' @param g_x a vector representing the number of subgroups for each group
#' @param rep_x a vector representing the number of replications in each
#' sub-group
#' @param n_a a vector representing the number of total replications in each
#' group
#' @param sig_a_sq a double representing the estimate of \deqn{\sigma^2_A}
#' @param sig_b_sq a double representing the estimate of \deqn{\sigma^2_B}
#' @param error_sq a double representing the estimate of \dqn{\sigma^2}
#' @param balanced a logical TRUE or FALSE indicating whether the design is
#' balanced or not
#' @param criteria a character "D" or "A" indicating the criteria to score the
#' experiment on
#'
#' @return a ggraph ggplot object
#' @export
#'
#' @examples
#' # Balanced design
#' plot_design_2(g = 2, g_x = c(2,2), rep_x = c(2,2,2,2), n_a = c(4, 4),
#' sig_a_sq = 2, sig_b_sq = 3, error_sq = 1, balanced = TRUE, criteria = "D")
#'
#' # Unbalanced design
#' plot_design_2(g = 2, g_x = c(2,3), rep_x = c(2,3,1,2,3), n_a = c(5, 6),
#' sig_a_sq = 2, sig_b_sq = 3, error_sq = 1, balanced = FALSE, criteria = "D")
plot_design_2 <- function(g, g_x, rep_x, n_a, sig_a_sq, sig_b_sq, error_sq, balanced, criteria) {
  if (criteria == "D") {
    crit = D_crit
  } else {
    crit = A_crit
  }

  from = c(rep("origin", g))
  for (i in seq_along(g_x)) {
    from <- append(from, rep(paste0("group", i), g_x[i]))
  }
  for (j in seq_along(rep_x)) {
    from <- append(from, rep(paste0("sub_group", j), rep_x[j]))
  }
  to <- c(unique(from)[-1])
  for (k in 1:sum(rep_x)) {
    to <- append(to, paste0("rep", k))
  }
  edges <- data.frame(first = from, second = to)

  name <- unique(c(as.character(edges$first), as.character(edges$second)))

  v_second <- c()
  for (l in seq_along(g_x)) {
    v_second <- append(v_second, rep(paste("group", l, sep = ""), g_x[l]))
  }

  v_third <- c()
  for (m in seq_along(rep_x)) {
    v_third <- append(v_third, rep(paste("sub_group", m, sep = ""), rep_x[m]))
  }

  vertices <- data.frame(
    name=name,
    group=c(rep(NA, g + 1), v_second, v_third))

  mygraph <- graph_from_data_frame(edges, vertices = vertices)

  if (balanced == FALSE) {
    title <- paste0("Unbalanced experiment with ", g, " groups")
  } else {
    title <- paste0("Balanced experiment with ", g,
                    " groups and ", g * g_x[1], " sub-groups")
  }
  info <- general_variance_3VC(N = sum(rep_x),
                               n_a = n_a,
                               n_b = rep_x,
                               sig_a_sq = sig_a_sq,
                               sig_b_sq = sig_b_sq,
                               error_sq = error_sq)
  score <- crit(info)

  ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
    geom_edge_elbow() +
    ggtitle(title) +
    geom_node_point(aes(filter=leaf, size = 2, color=group) , alpha=0.6) +
    labs(caption = paste0(criteria, " Score: ", signif(score, 6))) +
    theme_void() +
    theme(legend.position="none")
}
