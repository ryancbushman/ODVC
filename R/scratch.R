library(devtools)
library(ggplot2)
library(gridExtra)
library(partitions)
library(igraph)
library(ggraph)
library(statespacer)
library(gtools)
library(scales)

# #
# # # Balanced Data
# #
# # infomat <- function(a, n, tau, error) { # include big N
# #   tl <- 1 / (a * (n - 1))
# #   tr <- -1 / ((a * n) * (n - 1))
# #   bl <- tr
# #   br <- (1 / n^2) * (((1 + n * (tau))^2 / a) + (1 / (a * (n - 1))))
# #   elements <- c(tl, tr, bl, br)
# #   info <- (2 * (error)^2) * matrix(elements, nrow = 2, ncol = 2, byrow = TRUE)
# #   return(info)
# # }
# #
# # D_crit <- function(info){
# #   D.score <- det(info)
# #   return(D.score)
# # }
# #
# # A_crit <- function(info) {
# #   A.score <- sum(diag(info))
# #   return(A.score)
# # }
# #
# # balanced_designs <- data.frame("N" = double(100),
# #                                "a" = double(100),
# #                                "n" = character(100),
# #                                "tau" = double(100),
# #                                "Criteria" = double(100),
# #                                "Score" = double(100),
# #                                "Cross Score" = double(100),
# #                                "Releff" = double(100))
# #
# #
# #
# # ngroups <- c(5, 10, 20, 30)
# # nreps <- c(5, 10, 20)
# # taus <- c(0.5, 1, 2, 5)
# # balanced_contours_scores <- list()
# # balanced_contours_releff <- list()
# # balanced_contours_A_scores <- list()
# # balanced_contours_A_releff <- list()
# # count = 0
# #
# # for (i in seq_along(ngroups)) {
# #   for (j in seq_along(nreps)) {
# #     for (k in seq_along(taus)) {
# #       count = count + 1
# #
# #       x <- seq(2, ngroups[i], length.out = (ngroups[i]-1))
# #       y <- seq(2, nreps[j], length.out = (nreps[j] - 1))
# #
# #       grid <- expand.grid(x, y, stringsAsFactors = FALSE)
# #       grid$z <- numeric((ngroups[i]-1) * (nreps[j] - 1))
# #       for (l in seq_along(grid$z)) {
# #         grid$z[l] <- D_crit(infomat(grid[l, 1], grid[l, 2], taus[k], 1)) #replace with lambda as ratio
# #       }
# #       OD_score <- as.double(tail(grid, n=1)[3])
# #       grid$releff <- (OD_score / grid$z) * 100
# #
# #
# #       # add y=x line for reference
# #       #try 30 for number of groups and 20 for n as maxes
# #       contour_score <- ggplot(grid) +
# #         geom_contour_filled(data = grid,
# #                             aes(x = Var1, y = Var2,
# #                                 z = as.double(z)), color = "black") +
# #         xlim(0, ngroups[i]) +
# #         ylim(0, nreps[j]) +
# #         xlab("Number of Groups") +
# #         ylab("Reps per Group") +
# #         scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
# #         scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
# #         geom_abline(slope = 1, intercept = 0, color = "red") +
# #         ggtitle(paste0("D score when Tau = ", taus[k]))
# #
# #       contour_releff <- ggplot(grid) +
# #         geom_contour_filled(data = grid,
# #                             aes(x = Var1, y = Var2,
# #                                 z = as.double(releff)), color = "black") +
# #         xlim(0, ngroups[i]) +
# #         ylim(0, nreps[j]) +
# #         xlab("Number of Groups") +
# #         ylab("Reps per Group") +
# #         scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
# #         scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
# #         geom_abline(slope = 1, intercept = 0, color = "red") +
# #         ggtitle(paste0("Relative Efficiency when Tau = ", taus[k]))
# #
# #       balanced_contours_scores[[count]] <- contour_score
# #       balanced_contours_releff[[count]] <- contour_releff
# #
# #       balanced_designs$N[count] <- ngroups[i] * nreps[j]
# #       balanced_designs$a[count] <- ngroups[i]
# #       balanced_designs$n[count] <- nreps[j]
# #       balanced_designs$tau[count] <- taus[k]
# #       balanced_designs$Criteria[count] <- "D"
# #       balanced_designs$Score[count] <- grid$z[grid$Var1 == ngroups[i] & grid$Var2 == nreps[j]]
# #       balanced_designs$Cross.Score[count] <- A_crit(infomat(ngroups[i], nreps[j], taus[k], 1))
# #       balanced_designs$Releff[count] <- grid$releff[grid$Var1 == ngroups[i] & grid$Var2 == nreps[j]]
# #
# #     }
# #   }
# # }
# #
# # grid.arrange(balanced_contours_scores[[1]], balanced_contours_releff[[1]],
# #              balanced_contours_scores[[2]], balanced_contours_releff[[2]],
# #              balanced_contours_scores[[3]], balanced_contours_releff[[3]],
# #              balanced_contours_scores[[4]], balanced_contours_releff[[4]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[5]], balanced_contours_releff[[5]],
# #              balanced_contours_scores[[6]], balanced_contours_releff[[6]],
# #              balanced_contours_scores[[7]], balanced_contours_releff[[7]],
# #              balanced_contours_scores[[8]], balanced_contours_releff[[8]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[9]], balanced_contours_releff[[9]],
# #              balanced_contours_scores[[10]], balanced_contours_releff[[10]],
# #              balanced_contours_scores[[11]], balanced_contours_releff[[11]],
# #              balanced_contours_scores[[12]], balanced_contours_releff[[12]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[13]], balanced_contours_releff[[13]],
# #              balanced_contours_scores[[14]], balanced_contours_releff[[14]],
# #              balanced_contours_scores[[15]], balanced_contours_releff[[15]],
# #              balanced_contours_scores[[16]], balanced_contours_releff[[16]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[17]], balanced_contours_releff[[17]],
# #              balanced_contours_scores[[18]], balanced_contours_releff[[18]],
# #              balanced_contours_scores[[19]], balanced_contours_releff[[19]],
# #              balanced_contours_scores[[20]], balanced_contours_releff[[20]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[21]], balanced_contours_releff[[21]],
# #              balanced_contours_scores[[22]], balanced_contours_releff[[22]],
# #              balanced_contours_scores[[23]], balanced_contours_releff[[23]],
# #              balanced_contours_scores[[24]], balanced_contours_releff[[24]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[25]], balanced_contours_releff[[25]],
# #              balanced_contours_scores[[26]], balanced_contours_releff[[26]],
# #              balanced_contours_scores[[27]], balanced_contours_releff[[27]],
# #              balanced_contours_scores[[28]], balanced_contours_releff[[28]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[29]], balanced_contours_releff[[29]],
# #              balanced_contours_scores[[30]], balanced_contours_releff[[30]],
# #              balanced_contours_scores[[31]], balanced_contours_releff[[31]],
# #              balanced_contours_scores[[32]], balanced_contours_releff[[32]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[33]], balanced_contours_releff[[33]],
# #              balanced_contours_scores[[34]], balanced_contours_releff[[34]],
# #              balanced_contours_scores[[35]], balanced_contours_releff[[35]],
# #              balanced_contours_scores[[36]], balanced_contours_releff[[36]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[37]], balanced_contours_releff[[37]],
# #              balanced_contours_scores[[38]], balanced_contours_releff[[38]],
# #              balanced_contours_scores[[39]], balanced_contours_releff[[39]],
# #              balanced_contours_scores[[40]], balanced_contours_releff[[40]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[41]], balanced_contours_releff[[41]],
# #              balanced_contours_scores[[42]], balanced_contours_releff[[42]],
# #              balanced_contours_scores[[43]], balanced_contours_releff[[43]],
# #              balanced_contours_scores[[44]], balanced_contours_releff[[44]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_scores[[45]], balanced_contours_releff[[45]],
# #              balanced_contours_scores[[46]], balanced_contours_releff[[46]],
# #              balanced_contours_scores[[47]], balanced_contours_releff[[47]],
# #              balanced_contours_scores[[48]], balanced_contours_releff[[48]],
# #              ncol=4)
# #
# # for (i in seq_along(ngroups)) {
# #   for (j in seq_along(nreps)) {
# #     for (k in seq_along(taus)) {
# #       count = count + 1
# #
# #       x <- seq(2, ngroups[i], length.out = (ngroups[i]-1))
# #       y <- seq(2, nreps[j], length.out = (nreps[j] - 1))
# #
# #       grid <- expand.grid(x, y, stringsAsFactors = FALSE)
# #       grid$z <- numeric((ngroups[i]-1) * (nreps[j] - 1))
# #       for (l in seq_along(grid$z)) {
# #         grid$z[l] <- A_crit(infomat(grid[l, 1], grid[l, 2], taus[k], 1)) #replace with lambda as ratio
# #       }
# #       OD_score <- as.double(tail(grid, n=1)[3])
# #       grid$releff <- (OD_score / grid$z) * 100
# #
# #
# #       # add y=x line for reference
# #       #try 30 for number of groups and 20 for n as maxes
# #       contour_score <- ggplot(grid) +
# #         geom_contour_filled(data = grid,
# #                             aes(x = Var1, y = Var2,
# #                                 z = as.double(z)), color = "black") +
# #         xlim(0, ngroups[i]) +
# #         ylim(0, nreps[j]) +
# #         xlab("Number of Groups") +
# #         ylab("Reps per Group") +
# #         scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
# #         scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
# #         geom_abline(slope = 1, intercept = 0, color = "red") +
# #         ggtitle(paste0("A score when Tau = ", taus[k]))
# #
# #       contour_releff <- ggplot(grid) +
# #         geom_contour_filled(data = grid,
# #                             aes(x = Var1, y = Var2,
# #                                 z = as.double(releff)), color = "black") +
# #         xlim(0, ngroups[i]) +
# #         ylim(0, nreps[j]) +
# #         xlab("Number of Groups") +
# #         ylab("Reps per Group") +
# #         scale_x_continuous(breaks = seq(0, ngroups[i], 2)) +
# #         scale_y_continuous(breaks = seq(0, nreps[j], 2)) +
# #         geom_abline(slope = 1, intercept = 0, color = "red") +
# #         ggtitle(paste0("Relative Efficiency when Tau = ", taus[k]))
# #
# #       balanced_contours_A_scores[[count]] <- contour_score
# #       balanced_contours_A_releff[[count]] <- contour_releff
# #
# #       balanced_designs$N[count] <- ngroups[i] * nreps[j]
# #       balanced_designs$a[count] <- ngroups[i]
# #       balanced_designs$n[count] <- nreps[j]
# #       balanced_designs$tau[count] <- taus[k]
# #       balanced_designs$Criteria[count] <- "A"
# #       balanced_designs$Score[count] <- grid$z[grid$Var1 == ngroups[i] & grid$Var2 == nreps[j]]
# #       balanced_designs$Releff[count] <- grid$releff[grid$Var1 == ngroups[i] & grid$Var2 == nreps[j]]
# #
# #     }
# #   }
# # }
#
# # write as function, function recalculates relative efficiency where OD is of that subset
#
# # N100 <- balanced_designs[which(balanced_designs$N == 100),]
# # ggplot(data = N100) +
# #   geom_point(aes(x = a, y = Score,
# #                  color = tau)) +
# #   scale_color_gradient(low = "#ee9349", high = "black")
#
#   #cut(tau, c(-Inf, 0.5, 1,2 ,5 , Inf))
#
#   # scale_color_manual(name = "tau",
#   #                    values = c("(-Inf, 0.5]" = "#ee9349",
#   #                               "(0.5, 1]" = "#e45b49",
#   #                               "(1, 2]" = "#7c2349",
#   #                               "(2, 5]" = "black"),
#   #                    labels = c("0.5", "1", "2", "5"))
#
# # grid.arrange(balanced_contours_A_scores[[1]], balanced_contours_A_releff[[1]],
# #              balanced_contours_A_scores[[2]], balanced_contours_A_releff[[2]],
# #              balanced_contours_A_scores[[3]], balanced_contours_A_releff[[3]],
# #              balanced_contours_A_scores[[4]], balanced_contours_A_releff[[4]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[5]], balanced_contours_A_releff[[5]],
# #              balanced_contours_A_scores[[6]], balanced_contours_A_releff[[6]],
# #              balanced_contours_A_scores[[7]], balanced_contours_A_releff[[7]],
# #              balanced_contours_A_scores[[8]], balanced_contours_A_releff[[8]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[9]], balanced_contours_A_releff[[9]],
# #              balanced_contours_A_scores[[10]], balanced_contours_A_releff[[10]],
# #              balanced_contours_A_scores[[11]], balanced_contours_A_releff[[11]],
# #              balanced_contours_A_scores[[12]], balanced_contours_A_releff[[12]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[13]], balanced_contours_A_releff[[13]],
# #              balanced_contours_A_scores[[14]], balanced_contours_A_releff[[14]],
# #              balanced_contours_A_scores[[15]], balanced_contours_A_releff[[15]],
# #              balanced_contours_A_scores[[16]], balanced_contours_A_releff[[16]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[17]], balanced_contours_A_releff[[17]],
# #              balanced_contours_A_scores[[18]], balanced_contours_A_releff[[18]],
# #              balanced_contours_A_scores[[19]], balanced_contours_A_releff[[19]],
# #              balanced_contours_A_scores[[20]], balanced_contours_A_releff[[20]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[21]], balanced_contours_A_releff[[21]],
# #              balanced_contours_A_scores[[22]], balanced_contours_A_releff[[22]],
# #              balanced_contours_A_scores[[23]], balanced_contours_A_releff[[23]],
# #              balanced_contours_A_scores[[24]], balanced_contours_A_releff[[24]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[25]], balanced_contours_A_releff[[25]],
# #              balanced_contours_A_scores[[26]], balanced_contours_A_releff[[26]],
# #              balanced_contours_A_scores[[27]], balanced_contours_A_releff[[27]],
# #              balanced_contours_A_scores[[28]], balanced_contours_A_releff[[28]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[29]], balanced_contours_A_releff[[29]],
# #              balanced_contours_A_scores[[30]], balanced_contours_A_releff[[30]],
# #              balanced_contours_A_scores[[31]], balanced_contours_A_releff[[31]],
# #              balanced_contours_A_scores[[32]], balanced_contours_A_releff[[32]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[33]], balanced_contours_A_releff[[33]],
# #              balanced_contours_A_scores[[34]], balanced_contours_A_releff[[34]],
# #              balanced_contours_A_scores[[35]], balanced_contours_A_releff[[35]],
# #              balanced_contours_A_scores[[36]], balanced_contours_A_releff[[36]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[37]], balanced_contours_A_releff[[37]],
# #              balanced_contours_A_scores[[38]], balanced_contours_A_releff[[38]],
# #              balanced_contours_A_scores[[39]], balanced_contours_A_releff[[39]],
# #              balanced_contours_A_scores[[40]], balanced_contours_A_releff[[40]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[41]], balanced_contours_A_releff[[41]],
# #              balanced_contours_A_scores[[42]], balanced_contours_A_releff[[42]],
# #              balanced_contours_A_scores[[43]], balanced_contours_A_releff[[43]],
# #              balanced_contours_A_scores[[44]], balanced_contours_A_releff[[44]],
# #              ncol=4)
# #
# # grid.arrange(balanced_contours_A_scores[[45]], balanced_contours_A_releff[[45]],
# #              balanced_contours_A_scores[[46]], balanced_contours_A_releff[[46]],
# #              balanced_contours_A_scores[[47]], balanced_contours_A_releff[[47]],
# #              balanced_contours_A_scores[[48]], balanced_contours_A_releff[[48]],
# #              ncol=4)
#
#
#
#
# summary(grid$z)
# boxplot(grid$z)
# which.min(grid$z)
#
# # Unbalanced Data
#
# #install.packages("partitions")
# library(partitions)
#
#
# # define total number of experimental runs
# N      <- 8
#
# # partition total sample size (all possible)
# ps  <- parts(N)
#
# # compute the number of levels of experimental factor
# #  represented by each design/partitioning
# nZeros <- function(X, N) N - sum(X == 0)
#
#
# # define a and subset partitions to take only the
# #  relevant experimental designs
# getCols <- function(P, A){
#   n.lev.a   <- apply(P, 2, nZeros, N = N)
#   keep.cols <- which(n.lev.a == A)
#   return(P[,keep.cols])
# }
#
# # try it out
# getCols(P = ps, A = 1)
# getCols(P = ps, A = 2)
# getCols(P = ps, A = 3)
# getCols(P = ps, A = 4)
# getCols(P = ps, A = 5)
# getCols(P = ps, A = 6)
# getCols(P = ps, A = 7)
# getCols(P = ps, A = 8)
#
#
#
#
# # define total number of experimental runs
# N <- 30
#
# # partition total sample size (all possible)
# ps  <- parts(N)
#
# # try it out
# g2 <- getCols(P = ps, A = 2)
# g5 <- getCols(P = ps, A = 5)
# g10 <-getCols(P = ps, A = 10)
# g20 <- getCols(P = ps, A = 20)
#
# # unbalanced information matrix
#
# u_infomat <- function(N, a, n_i, vc, error) {
#   lambda_i <- error + n_i * vc
#   D <- ((N - a) / error^2) * sum((n_i / lambda_i)^2) +
#     sum(1 / lambda_i^2) * sum((n_i / lambda_i)^2) -
#     (sum(n_i / lambda_i^2))^2
#   tl <- sum((n_i / lambda_i)^2)
#   tr <- - sum(n_i / lambda_i^2)
#   bl <- tr
#   br <- ((N - a) / error^2) + sum(1 / lambda_i^2)
#   elements <- c(tl, tr, bl, br)
#   info <- (2 / D) * matrix(elements, 2, 2, byrow = TRUE)
#   return(info)
# }
#
# # test
# u_infomat(10, 4, c(1,2,3,4), 2, 1)
#
# g2 <- g2[1:2,]
# g5 <- g5[1:5,]
# g10 <- g10[1:10,]
# g20 <- g20[1:20,]
#
#
#
# unbalanced_designs <- data.frame("N" = double(10000),
#                                  "a" = double(10000),
#                                  "n_i" = character(10000),
#                                  "vc" = double(10000),
#                                  "A Score" = double(10000),
#                                  "D Score" = double(10000))
# count = 0
#
# # a = 2
# for (i in seq_along(taus)) {
#   for (j in 1:ncol(g2)) {
#     count = count + 1
#     info <- u_infomat(N = N, a = 2, n_i = g2[1:2, j], taus[i], 1)
#     unbalanced_designs$N[count] = 30
#     unbalanced_designs$a[count] = 2
#     unbalanced_designs$n_i[count] = paste0(g2[1, j], ", ", g2[2, j])
#     unbalanced_designs$vc[count] = taus[i]
#     unbalanced_designs$A.Score[count] = A_crit(info)
#     unbalanced_designs$D.Score[count] = D_crit(info)
#   }
# }
#
# # a = 5
# for (i in seq_along(taus)) {
#   for (j in 1:ncol(g5)) {
#     count = count + 1
#     info <- u_infomat(N = N, a = 5, n_i = g5[1:5, j], taus[i], 1)
#     unbalanced_designs$N[count] = 30
#     unbalanced_designs$a[count] = 5
#     unbalanced_designs$n_i[count] = paste0(g5[1, j], ", ", g5[2, j], ", ",
#                                            g5[3, j], ", ", g5[4, j], ", ",
#                                            g5[5, j])
#     unbalanced_designs$vc[count] = taus[i]
#     unbalanced_designs$A.Score[count] = A_crit(info)
#     unbalanced_designs$D.Score[count] = D_crit(info)
#   }
# }
#
# # a = 10
# for (i in seq_along(taus)) {
#   for (j in 1:ncol(g10)) {
#     count = count + 1
#     info <- u_infomat(N = N, a = 10, n_i = g10[1:10, j], taus[i], 1)
#     unbalanced_designs$N[count] = 30
#     unbalanced_designs$a[count] = 10
#     unbalanced_designs$n_i[count] = paste0(g10[1, j], ", ", g10[2, j], ", ",
#                                            g10[3, j], ", ", g10[4, j], ", ",
#                                            g10[5, j], ", ", g10[6, j], ", ",
#                                            g10[7, j], ", ", g10[8, j], ", ",
#                                            g10[9, j], ", ", g10[10, j])
#     unbalanced_designs$vc[count] = taus[i]
#     unbalanced_designs$A.Score[count] = A_crit(info)
#     unbalanced_designs$D.Score[count] = D_crit(info)
#   }
# }
#
# # a = 20
# for (i in seq_along(taus)) {
#   for (j in 1:ncol(g20)) {
#     count = count + 1
#     info <- u_infomat(N = N, a = 20, n_i = g20[1:20, j], taus[i], 1)
#     unbalanced_designs$N[count] = 30
#     unbalanced_designs$a[count] = 20
#     unbalanced_designs$n_i[count] = paste0(g20[1, j], ", ", g20[2, j], ", ",
#                                            g20[3, j], ", ", g20[4, j], ", ",
#                                            g20[5, j], ", ", g20[6, j], ", ",
#                                            g20[7, j], ", ", g20[8, j], ", ",
#                                            g20[9, j], ", ", g20[10, j], ", ",
#                                            g20[11, j], ", ", g20[12, j], ", ",
#                                            g20[13, j], ", ", g20[14, j], ", ",
#                                            g20[15, j], ", ", g20[16, j], ", ",
#                                            g20[17, j], ", ", g20[18, j], ", ",
#                                            g20[19, j], ", ", g20[20, j])
#     unbalanced_designs$vc[count] = taus[i]
#     unbalanced_designs$A.Score[count] = A_crit(info)
#     unbalanced_designs$D.Score[count] = D_crit(info)
#   }
# }
#
# unbalanced_designs[3855, 1:6]



# Hes <- function(error, vc, n, N){
#   tl <- (1 / error^4) - ((2 * n * vc^2) / (n * vc^2 * error^4 + error^6)) + ((n^2 * vc^4) / (n * vc^2 * error^2 + error^4)^2)
#   tr <- (n / error^4) - ((2 * n^2 * vc^2) / (n * vc^2 * error*4 + error^6)) + ((n^3 * vc^4) / (n * vc^2 * error^2 + error^4)^2)
#   bl <- tr
#   br <- (n^2 / error^4) - ((2 * n^3 * vc^2) / (n * vc^2 * error^4 + error^6)) + ((n^4 * vc^4) / (n * vc^2 * error^4 + error^6)^2)
#   elements <- c(tl, tr, bl, br)
#   info <- matrix(elements, nrow = 2, ncol = 2, byrow = TRUE)
#   info <- (N / 2) * info
#   return(info)
# }
#
# Hes2 <- function(error, vc, n, N) {
#   v_inv <- (1 / error^2) - ((n * vc^2) / (n * vc^2 * error^2 + error^4))
#   tl <- (N / 2) * v_inv * v_inv
#   tr <- (N / 2) * v_inv * v_inv * n
#   bl <- tr
#   br <- (N / 2) * v_inv * n * v_inv * n
#   elements <- c(tl, tr, bl, br)
#   info <- matrix(elements, nrow = 2, ncol = 2, byrow = TRUE)
#   return(info)
# }
#
# test1 <- one_way_cov_B(error = 1, tau = 1, 10, 10)
#
# trial <- Hes(1, 1, 10, 100)
# test2 <- solve(trial)

# My function is close to the output of the searle dispersion matrix, but the elements are mixed around
#
# der_V_sig_a_sq <- function(n, g, sig_a_sq) {
#   kronecker(diag(1, g), matrix(rep(1, n * n), nrow = n, ncol = n))
# }
#
# J_n <- function(n) {
#   (1 / n) * matrix(rep(1, n * n), nrow = n, ncol = n)
# }
#
# V <- function(n, g, sig_a_sq, error_sq) {
#   first <- diag(1, nrow = g)
#   second <- (n * sig_a_sq) * J_n(n)
#   third <- error_sq * diag(1, nrow = n)
#   kronecker(first, (second + third))
# }
#
# Hes3 <- function(n, g, sig_a_sq, error_sq) {
#   temp_V <- V(n = n, g = g, sig_a_sq = sig_a_sq, error_sq = error_sq)
#   temp_der <- der_V_sig_a_sq(n = n, g = g, sig_a_sq = sig_a_sq)
#   t_l <- (1 / 2) * sum(diag(solve(temp_V) %*% solve(temp_V)))
#   t_r <- (1 / 2) * sum(diag(solve(temp_V) %*% temp_der %*% solve(temp_V)))
#   b_l <- (1 / 2) * sum(diag(solve(temp_V) %*% solve(temp_V) %*% temp_der))
#   b_r <- (1 / 2) * sum(diag(solve(temp_V) %*% temp_der %*% solve(temp_V) %*% temp_der))
#
#   matrix(c(t_l, t_r, b_l, b_r), nrow = 2, ncol = 2)
# }
#
# Hes3(n = 10, g = 15, sig_a_sq = 5, error_sq = 1)
# solve(one_way_cov_B(error = 1, tau = 5, a = 15, n = 10))
#
# # I think I got my derivative of V with respect to sig_sq_A wrong. Factor out the 1/n from J_n and the derivative would just be kronecker(I_g, J_n)
# #
# # n_children <- 4
# # max_depth <- 3
# # regular_n <- 60#sum(n_children^c(0:max_depth))
# # regular_org <- tidygraph::create_tree(n = regular_n, children = n_children)
# # ggraph(regular_org, 'dendrogram') + geom_edge_diagonal() +
# #   geom_node_point(shape = 21, size = 2, fill = 'white') + theme_bw()
# #
# # a = 4
# # b = c(2, 1, 2, 1)
# # d = c(1, 2, 3, 4, 5, 6)
# #
# # groups <- data.frame(from="origin", to=paste("group", seq(1, a), sep=""))
# # reps_per_groups1 <- data.frame(from=rep(groups$to, b),
# #                               to=paste("rep", seq(1,sum(b)), sep="_"))
# # reps_per_groups2 <- data.frame(from=rep(reps_per_groups1$from, d), to = paste("rep", seq(b+1, b+sum(d)), sep = "_"))
# # edges <- rbind(groups, reps_per_groups1, reps_per_groups2)
# #
# # name <- unique(c(as.character(edges$from), as.character(edges$to)))
# # vertices <- data.frame(
# #   name=name,
# #   group=c( rep(NA, g + 1) , rep(paste("group", seq(1, g), sep=""), n))
# # )
# #
# # mygraph <- graph_from_data_frame( edges, vertices=vertices)
# #
# # ggraph(mygraph, layout = "dendrogram", circular = FALSE) + geom_edge_elbow()
# #
# # from = c("origin", "origin", "group1", "group1", "group2", "group2", "group1_1",
# #          "group1_1", "group1_2", "group1_2", "group2_1", "group2_1", "group2_2", "group2_2")
# # to = c("group1", "group2", "group1_1", "group1_2", "group2_1", "group2_2", "rep1", "rep2", "rep3", "rep4", "rep5",
# #        "rep6", "rep7", "rep8")
# # edges_test = data.frame(first = from, second = to)
# #
# # name = c("origin", "group1", "group2", "group1_1", "group1_2", "group2_1",
# #          "group2_2", "rep1", "rep2", "rep3", "rep4", "rep5",
# #          "rep6", "rep7", "rep8")
# # group = c(NA, NA, NA, "group1", "group1", "group2", "group2", "group1_1",
# #           "group1_1", "group1_2", "group1_2", "group2_1", "group2_1",
# #           "group2_2", "group2_2")
# # vertices_test <- data.frame(third = name, fourth = group)
# #
# # test_graph <- graph_from_data_frame(edges_test, vertices = vertices_test)
# # ggraph(test_graph, layout = "dendrogram", circular = FALSE) + geom_edge_elbow()
#
# # Attempting to verify using row 1 of table 4.2 from Delgado Dissertation, pg 81
# test <- general_variance_3VC(16, c(4, 4, 4, 4), c(2, 2, 2, 2, 2, 2, 2, 2), 1, 1, 8)
# test1 <- matrix(c(test[1,1],
#                   test[1,2] / 0.125,
#                   test[1,3] / 0.125,
#                   test[1,2] / 0.125,
#                   test[2,2] / 0.125^2,
#                   test[2,3] / 0.125^2,
#                   test[1,3] / 0.125,
#                   test[2,2] / 0.125^2,
#                   test[3,3] / 0.125^2), nrow = 3, ncol = 3, byrow = TRUE)
# 16 * A_crit(test1)
#
# plot_design_2(4, c(2, 2, 2, 2), c(2, 2, 2, 2, 2, 2, 2, 2), c(4, 4, 4, 4), 1, 1, 8, TRUE, "D")
#
# test <- general_variance_2VC(120, 8, 15, 1, 8)
# D_crit(test)
#
# c_i <- 2
# a <- 2
# n <- 4
# sig_a_sq <- 1
# sig_b_sq <- 1
# error_sq <- 1
# x <- a*(c_i - 1) / (n*sig_b_sq + error_sq)^2
# y <- a / (c_i * n * sig_a_sq + n * sig_b_sq + error_sq)^2
# z = a * c_i * (n - 1) / error_sq^2
#
# T_mat <- matrix(c(c_i^2 * n^2 *y,
#                   c_i * n^2 * y,
#                   c_i * n * y,
#                   c_i * n^2 * y,
#                   n^2 * (x + y),
#                   n * (x + y),
#                   c_i * n * y,
#                   n * (x + y),
#                   x + y + z), nrow = 3, ncol = 3, byrow = TRUE)
# T_mat
# solve(T_mat)
# general_variance_3VC(8, c(4, 4), c(2, 2, 2, 2), 1, 1, 1)
# solve(general_variance_3VC(8, c(4, 4), c(2, 2, 2, 2), 1, 1, 1))
#
#
# ######## generate_designs_3VC_U Scratch Code ##############################
#
#
