#' plot_design_2
#'
#' plot_design_2 plots a dendrogram representation of the design of a user
#' specified two-way nested random effects experiment.
#'
#' @param a an integer representing the number of groups
#' @param b_i a vector representing the number of subgroups for each group
#' @param n_ij a vector representing the number of replications in each
#' sub-group
#' @param n_i_dot a vector representing the number of total replications in each
#' group
#' @param sig_a_sq a double representing the hypothesized value of \eqn{\sigma^2_A}
#' @param sig_b_sq a double representing the hypothesized value of \eqn{\sigma^2_B}
#' @param error_sq a double representing the hypothesized value of \eqn{\sigma^2}
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
#' plot_design_2(a = 2, b_i = c(2,2), n_ij = c(2,2,2,2), n_i_dot = c(4, 4),
#' sig_a_sq = 2, sig_b_sq = 3, error_sq = 1, balanced = TRUE, criteria = "D")
#'
#' # Unbalanced design
#' plot_design_2(a = 2, b_i = c(2,3), n_ij = c(2,3,1,2,3), n_i_dot = c(5, 6),
#' sig_a_sq = 2, sig_b_sq = 3, error_sq = 1, balanced = FALSE, criteria = "D")
plot_design_2 <- function(a, b_i, n_ij, n_i_dot, sig_a_sq, sig_b_sq, error_sq, balanced, criteria) {
  if (criteria == "D") {
    crit = D_crit
  } else {
    crit = A_crit
  }

  from = c(rep("origin", a))
  for (i in seq_along(b_i)) {
    from <- append(from, rep(paste0("group", i), b_i[i]))
  }
  for (j in seq_along(n_ij)) {
    from <- append(from, rep(paste0("sub_group", j), n_ij[j]))
  }
  to <- c(unique(from)[-1])
  for (k in 1:sum(n_ij)) {
    to <- append(to, paste0("rep", k))
  }
  edges <- data.frame(first = from, second = to)

  name <- unique(c(as.character(edges$first), as.character(edges$second)))

  v_second <- c()
  for (l in seq_along(b_i)) {
    v_second <- append(v_second, rep(paste("group", l, sep = ""), b_i[l]))
  }

  v_third <- c()
  for (m in seq_along(n_ij)) {
    v_third <- append(v_third, rep(paste("sub_group", m, sep = ""), n_ij[m]))
  }

  vertices <- data.frame(
    name=name,
    group=c(rep(NA, a + 1), v_second, v_third))

  mygraph <- igraph::graph_from_data_frame(edges, vertices = vertices)

  if (balanced == FALSE) {
    title <- paste0("Unbalanced experiment with ", a, " groups")
  } else {
    title <- paste0("Balanced experiment with ", a,
                    " groups and ", a * b_i[1], " sub-groups")
  }
  info <- general_variance_3VC(N = sum(n_ij),
                               n_i_dot = n_i_dot,
                               n_ij = n_ij,
                               sig_a_sq = sig_a_sq,
                               sig_b_sq = sig_b_sq,
                               error_sq = error_sq)
  score <- crit(info)

  ggraph::ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
    ggraph::geom_edge_elbow() +
    ggtitle(title) +
    ggraph::geom_node_point(aes(filter=leaf, size = 2, color=group) , alpha=0.6) +
    labs(caption = paste0(criteria, " Score: ", signif(score, 6))) +
    theme_void() +
    theme(legend.position="none")
}
