# # x <- seq(2, 5, length.out = 4)
# # y.test <- seq(2, 5, length.out = 4)
# #
# # grid.test <- expand.grid(x.test, y.test, stringsAsFactors = FALSE)
# # grid.test$z <- numeric(16)
# # for (l in seq_along(grid.test$z)) {
# #   grid.test$z[l] <- A_crit(infomat(grid.test[l, 1], grid.test[l, 2], 5, 1)) #replace with lambda as ratio
# # }
# # OD_score.test <- as.double(tail(grid.test, n=1)[3])
# # grid.test$releff <- (OD_score.test / grid.test$z) * 100
# #
# # OD_t <- c(rep(OD_5_5, 4), rep(OD_5_10, 4), rep(OD_5_20, 4),
# #           rep(OD_10_5, 4), rep(OD_10_10, 4), rep(OD_10_20, 4),
# #           rep(OD_20_5, 4), rep(OD_20_10, 4), rep(OD_20_20, 4),
# #           rep(OD_30_5, 4), rep(OD_30_10, 4), rep(OD_30_20, 4))
#
# library(ggplot2)
# library(gridExtra)
#
# # Balanced Data
#
# infomat <- function(a, n, tau, error) { # include big N
#   tl <- 1 / (a * (n - 1))
#   tr <- -1 / ((a * n) * (n - 1))
#   bl <- tr
#   br <- (1 / n^2) * (((1 + n * (tau))^2 / a) + (1 / (a * (n - 1))))
#   elements <- c(tl, tr, bl, br)
#   info <- (2 * (error)^2) * matrix(elements, nrow = 2, ncol = 2, byrow = TRUE)
#   return(info)
# }
#
# D_crit <- function(info){
#   D.score <- det(info)
#   return(D.score)
# }
#
# A_crit <- function(info) {
#   A.score <- sum(diag(info))
#   return(A.score)
# }
#
#
#
# ngroups <- c(5, 10, 20, 30)
# nreps <- c(5, 10, 20)
# taus <- c(0.5, 1, 2, 5)
# balanced_contours_scores <- list()
# balanced_contours_releff <- list()
# balanced_contours_A_scores <- list()
# balanced_contours_A_releff <- list()
# count = 0
#
# OD_5_5 <- D_crit(infomat(5, 5, 0.5, 1))
# OD_5_10 <- D_crit(infomat(5, 10, 0.5, 1))
# OD_5_20 <- D_crit(infomat(5, 20, 0.5, 1))
# OD_10_5 <- D_crit(infomat(10, 5, 0.5, 1))
# OD_10_10 <- D_crit(infomat(10, 10, 0.5, 1))
# OD_10_20 <- D_crit(infomat(10, 20, 0.5, 1))
# OD_20_5 <- D_crit(infomat(20, 5, 0.5, 1))
# OD_20_10 <- D_crit(infomat(20, 10, 0.5, 1))
# OD_20_20 <- D_crit(infomat(20, 20, 0.5, 1))
# OD_30_5 <- D_crit(infomat(30, 5, 0.5, 1))
# OD_30_10 <- D_crit(infomat(30, 10, 0.5, 1))
# OD_30_20 <- D_crit(infomat(30, 20, 0.5, 1))
#
#
#
#
# for (i in seq_along(ngroups)) {
#   for (j in seq_along(nreps)) {
#     for (k in seq_along(taus)) {
#       count = count + 1
#
#       x <- seq(2, ngroups[i], length.out = (ngroups[i]-1))
#       y <- seq(2, nreps[j], length.out = (nreps[j] - 1))
#
#       grid <- expand.grid(x, y, stringsAsFactors = FALSE)
#       grid$z <- numeric((ngroups[i]-1) * (nreps[j] - 1))
#       for (l in seq_along(grid$z)) {
#         grid$z[l] <- D_crit(infomat(grid[l, 1], grid[l, 2], taus[k], 1)) #replace with lambda as ratio
#       }
#
#       if (ngroups[i] == 5 & nreps[j] == 5) {
#         OD_score <- OD_5_5
#       } else if (ngroups[i] == 5 & nreps[j] == 10) {
#         OD_score <- OD_5_10
#       } else if (ngroups[i] == 5 & nreps[j] == 20) {
#         OD_score <- OD_5_20
#       }else if (ngroups[i] == 10 & nreps[j] == 5) {
#         OD_score <- OD_10_5
#       }else if (ngroups[i] == 10 & nreps[j] == 10) {
#         OD_score <- OD_10_10
#       }else if (ngroups[i] == 10 & nreps[j] == 20) {
#         OD_score <- OD_10_20
#       }else if (ngroups[i] == 20 & nreps[j] == 5) {
#         OD_score <- OD_20_5
#       }else if (ngroups[i] == 20 & nreps[j] == 10) {
#         OD_score <- OD_20_10
#       }else if (ngroups[i] == 20 & nreps[j] == 20) {
#         OD_score <- OD_20_20
#       }else if (ngroups[i] == 30 & nreps[j] == 5) {
#         OD_score <- OD_30_5
#       }else if (ngroups[i] == 30 & nreps[j] == 10) {
#         OD_score <- OD_30_10
#       }else if (ngroups[i] == 30 & nreps[j] == 20) {
#         OD_score <- OD_30_20
#       }
#
#       grid$releff <- (OD_score / grid$z) * 100
#
#
#       # add y=x line for reference
#       #try 30 for number of groups and 20 for n as maxes
#       contour_score <- ggplot(grid) +
#         geom_contour_filled(data = grid,
#                             aes(x = Var1, y = Var2,
#                                 z = as.double(z)), color = "black") +
#         xlim(0, ngroups[i]) +
#         ylim(0, nreps[j]) +
#         xlab("Number of Groups") +
#         ylab("Reps per Group") +
#         scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
#         scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
#         geom_abline(slope = 1, intercept = 0, color = "red") +
#         ggtitle(paste0("D score when Tau = ", taus[k]))
#
#       contour_releff <- ggplot(grid) +
#         geom_contour_filled(data = grid,
#                             aes(x = Var1, y = Var2,
#                                 z = as.double(releff)), color = "black",
#                             breaks = seq(0, 100, 10)) +
#         xlim(0, ngroups[i]) +
#         ylim(0, nreps[j]) +
#         xlab("Number of Groups") +
#         ylab("Reps per Group") +
#         scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
#         scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
#         geom_abline(slope = 1, intercept = 0, color = "red") +
#         ggtitle(paste0("Relative Efficiency when Tau = ", taus[k]))
#
#       balanced_contours_scores[[count]] <- contour_score
#       balanced_contours_releff[[count]] <- contour_releff
#
#     }
#   }
# }
#
# grid.arrange(balanced_contours_scores[[1]], balanced_contours_releff[[1]],
#              balanced_contours_scores[[2]], balanced_contours_releff[[2]],
#              balanced_contours_scores[[3]], balanced_contours_releff[[3]],
#              balanced_contours_scores[[4]], balanced_contours_releff[[4]],
#              ncol=4)
#
# grid.arrange(balanced_contours_scores[[45]], balanced_contours_releff[[45]],
#              balanced_contours_scores[[46]], balanced_contours_releff[[46]],
#              balanced_contours_scores[[47]], balanced_contours_releff[[47]],
#              balanced_contours_scores[[48]], balanced_contours_releff[[48]],
#              ncol=4)
#
# OD_A_5_5 <- A_crit(infomat(5, 5, 0.5, 1))
# OD_A_5_10 <- A_crit(infomat(5, 10, 0.5, 1))
# OD_A_5_20 <- A_crit(infomat(5, 20, 0.5, 1))
# OD_A_10_5 <- A_crit(infomat(10, 5, 0.5, 1))
# OD_A_10_10 <- A_crit(infomat(10, 10, 0.5, 1))
# OD_A_10_20 <- A_crit(infomat(10, 20, 0.5, 1))
# OD_A_20_5 <- A_crit(infomat(20, 5, 0.5, 1))
# OD_A_20_10 <- A_crit(infomat(20, 10, 0.5, 1))
# OD_A_20_20 <- A_crit(infomat(20, 20, 0.5, 1))
# OD_A_30_5 <- A_crit(infomat(30, 5, 0.5, 1))
# OD_A_30_10 <- A_crit(infomat(30, 10, 0.5, 1))
# OD_A_30_20 <- A_crit(infomat(30, 20, 0.5, 1))
#
# for (i in seq_along(ngroups)) {
#   for (j in seq_along(nreps)) {
#     for (k in seq_along(taus)) {
#       count = count + 1
#
#       x <- seq(2, ngroups[i], length.out = (ngroups[i]-1))
#       y <- seq(2, nreps[j], length.out = (nreps[j] - 1))
#
#       grid <- expand.grid(x, y, stringsAsFactors = FALSE)
#       grid$z <- numeric((ngroups[i]-1) * (nreps[j] - 1))
#       for (l in seq_along(grid$z)) {
#         grid$z[l] <- A_crit(infomat(grid[l, 1], grid[l, 2], taus[k], 1)) #replace with lambda as ratio
#       }
#
#       if (ngroups[i] == 5 & nreps[j] == 5) {
#         OD_score <- OD_A_5_5
#       } else if (ngroups[i] == 5 & nreps[j] == 10) {
#         OD_score <- OD_A_5_10
#       } else if (ngroups[i] == 5 & nreps[j] == 20) {
#         OD_score <- OD_A_5_20
#       }else if (ngroups[i] == 10 & nreps[j] == 5) {
#         OD_score <- OD_A_10_5
#       }else if (ngroups[i] == 10 & nreps[j] == 10) {
#         OD_score <- OD_A_10_10
#       }else if (ngroups[i] == 10 & nreps[j] == 20) {
#         OD_score <- OD_A_10_20
#       }else if (ngroups[i] == 20 & nreps[j] == 5) {
#         OD_score <- OD_A_20_5
#       }else if (ngroups[i] == 20 & nreps[j] == 10) {
#         OD_score <- OD_A_20_10
#       }else if (ngroups[i] == 20 & nreps[j] == 20) {
#         OD_score <- OD_A_20_20
#       }else if (ngroups[i] == 30 & nreps[j] == 5) {
#         OD_score <- OD_A_30_5
#       }else if (ngroups[i] == 30 & nreps[j] == 10) {
#         OD_score <- OD_A_30_10
#       }else if (ngroups[i] == 30 & nreps[j] == 20) {
#         OD_score <- OD_A_30_20
#       }
#
#       grid$releff <- (OD_score / grid$z) * 100
#
#
#       # add y=x line for reference
#       #try 30 for number of groups and 20 for n as maxes
#       contour_score <- ggplot(grid) +
#         geom_contour_filled(data = grid,
#                             aes(x = Var1, y = Var2,
#                                 z = as.double(z)), color = "black") +
#         xlim(0, ngroups[i]) +
#         ylim(0, nreps[j]) +
#         xlab("Number of Groups") +
#         ylab("Reps per Group") +
#         scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
#         scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
#         geom_abline(slope = 1, intercept = 0, color = "red") +
#         ggtitle(paste0("A score when Tau = ", taus[k]))
#
#       contour_releff <- ggplot(grid) +
#         geom_contour_filled(data = grid,
#                             aes(x = Var1, y = Var2,
#                                 z = as.double(releff)), color = "black",
#                             breaks = seq(0, 100, 10)) +
#         xlim(0, ngroups[i]) +
#         ylim(0, nreps[j]) +
#         xlab("Number of Groups") +
#         ylab("Reps per Group") +
#         scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
#         scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
#         geom_abline(slope = 1, intercept = 0, color = "red") +
#         ggtitle(paste0("Relative Efficiency when Tau = ", taus[k]))
#
#       balanced_contours_A_scores[[count]] <- contour_score
#       balanced_contours_A_releff[[count]] <- contour_releff
#
#     }
#   }
# }
#
# grid.arrange(balanced_contours_A_scores[[1]], balanced_contours_A_releff[[1]],
#              balanced_contours_A_scores[[2]], balanced_contours_A_releff[[2]],
#              balanced_contours_A_scores[[3]], balanced_contours_A_releff[[3]],
#              balanced_contours_A_scores[[4]], balanced_contours_A_releff[[4]],
#              ncol=4)
#
# grid.arrange(balanced_contours_A_scores[[45]], balanced_contours_A_releff[[45]],
#              balanced_contours_A_scores[[46]], balanced_contours_A_releff[[46]],
#              balanced_contours_A_scores[[47]], balanced_contours_A_releff[[47]],
#              balanced_contours_A_scores[[48]], balanced_contours_A_releff[[48]],
#              ncol=4)
