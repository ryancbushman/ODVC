#' contour_designs_B
#'
#' contour_designs_B creates a list of size two in which both elements are a
#' list of contour plots. The first element is a list of contour plots comparing
#' criteria score across user specified ranges of designs. The second element is
#' a list of contour plots comparing relative efficiencies across user specified
#' ranges of designs. The relative efficiencies are calculated in comparison to
#' the design with the lowest criteria score for the range of designs on the
#' specific plot. The purpose of this tool is to allow users to compare how
#' running an experiment with a different number of groups or replications per
#' group can result in a comparable design performance. This can be useful if
#' running an experiment in a comparable way is more cost effective.
#'
#' NOTE: This function will return a list of two lists of length
#' (ngroups)(nreps)(taus). It is best to save the output of this function to
#' a variable in your global environment. See example below.
#'
#' @param ngroups a vector of integers indicating the number of groups to cross
#' with number of replications per group
#' @param nreps a vector of integers indicating the number of replications per
#' group to cross with number of groups
#' @param taus a vector of doubles indicating the values of tau to
#' cross with the number of crossed groups by replications
#' @param error_sq a double indicating the estimated residual error of the model.
#' Default value of 1 is strongly encouraged to make tau easy to interpret.
#' @param criteria a character "D" or "A" indicating which design criteria to
#' use
#'
#' @return a list of two lists; the first being a list containing contour plots
#' that compare criteria scores and the second being a list containing contour
#' plots that compare relative efficiencies
#' @export
#'
#' @examples
#'
#' candidate_designs <- contour_designs_B(ngroups = c(10, 20),
#'                                         nreps = c(10, 20),
#'                                         taus = c(0.5, 1, 2),
#'                                         error_sq = 1,
#'                                         criteria = "D")
#'
contour_designs_B <- function(ngroups = c(5, 10, 20, 30),
                              nreps = c(5, 10, 20),
                              taus = c(0.5, 1, 2, 5),
                              error_sq = 1, criteria = "D"){
  if (!all(floor(ngroups) == ngroups)) {
    stop("ngroups must be a single integer or vector of integers")
  }
  if (!all(nreps == floor(nreps))) {
    stop("nreps must be a single integer or vector of integers")
  }
  if (!is.vector(taus) & !is.numeric(taus)) {
    stop("taus must be a single numeric or vector of numerics")
  }
  if (!is.numeric(error_sq)) {
    stop("error_sq must be a numeric")
  }
  if (length(error_sq) != 1) {
    stop("error_sq must be a single numeric value")
  }
  if (!(criteria %in% c("A", "D"))) {
    stop("criteria must be either the string A or D")
  }

  iterations <- (length(ngroups) * length(nreps) * length(taus))
  balanced_contours_scores <- vector(mode = "list", length = iterations)
  balanced_contours_releff <- vector(mode = "list", length = iterations)
  if (criteria == "D") {
    crit <- D_crit
  } else if (criteria == "A") {
    crit <- A_crit
  }

  count = 0
  for (i in seq_along(ngroups)) {
    for (j in seq_along(nreps)) {
      for (k in seq_along(taus)) {
        count = count + 1

        x <- seq(2, ngroups[i], length.out = (ngroups[i]-1))
        y <- seq(2, nreps[j], length.out = (nreps[j] - 1))

        grid <- expand.grid(x, y, stringsAsFactors = FALSE)
        grid$z <- numeric((ngroups[i]-1) * (nreps[j] - 1))
        for (l in seq_along(grid$z)) {
          grid$z[l] <- crit(general_variance_2VC(N = grid[l, 1] * grid[l, 2],
                                                 n = grid[l, 2], a = grid[l, 1],
                                                 sig_a_sq = taus[k],
                                                 error_sq = 1))
          # grid$z[l] <- crit(one_way_cov_B(error_sq = 1,
          #                                 tau = taus[k],
          #                                 grid[l, 1],
          #                                 grid[l, 2]))
        }
        OD_score <- as.double(tail(grid, n=1)[3])
        grid$releff <- (OD_score / grid$z) * 100

        contour_score <-  suppressMessages(
          print(
            ggplot(grid) +
              geom_contour_filled(data = grid,
                                  aes(x = Var1, y = Var2,
                                      z = as.double(z)), color = "black") +
              xlim(0, ngroups[i]) +
              ylim(0, nreps[j]) +
              xlab("Number of Groups") +
              ylab("Reps per Group") +
              scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
              scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
              geom_abline(slope = 1, intercept = 0, color = "red") +
              ggtitle(paste0("D score when Tau = ", taus[k]))))

        contour_releff <- suppressMessages(print(ggplot(grid) +
                                                   geom_contour_filled(data = grid,
                                                                       aes(x = Var1, y = Var2,
                                                                           z = as.double(releff)), color = "black") +
                                                   xlim(0, ngroups[i]) +
                                                   ylim(0, nreps[j]) +
                                                   xlab("Number of Groups") +
                                                   ylab("Reps per Group") +
                                                   scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
                                                   scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
                                                   geom_abline(slope = 1, intercept = 0, color = "red") +
                                                   ggtitle(paste0("Relative Efficiency when Tau = ", taus[k]))))

        balanced_contours_scores[[count]] <- contour_score
        balanced_contours_releff[[count]] <- contour_releff
      }
    }
  }
  score_releff_plots <- list(balanced_contours_scores, balanced_contours_releff)
  return(score_releff_plots)
}
