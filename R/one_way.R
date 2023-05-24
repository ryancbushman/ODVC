library(ggplot2)

infomat <- function(a, n, vc, error) {
  tl <- 1 / (a * (n - 1))
  tr <- -1 / ((a * n) * (n - 1))
  bl <- tr
  br <- (1 / n^2) * (((1 + n * (vc / error)) / a) + (1 / (a * (n - 1))))
  elements <- c(tl, tr, bl, br)
  info <- matrix(elements, nrow = 2, ncol = 2, byrow = TRUE)
  return(info)
}

D_crit <- function(info){
    det.inf <- det(info)
  if (det.inf < (.Machine$double.eps)^(0.5)) {
    D.score <- Inf
  } else {
    D.score <- det.inf
  }
  return(D.score)
}

D_crit <- function(info){
  D.score <- det(info)
  return(D.score)
}

x <- seq(1, 10, length.out = 10)
y <- seq(1, 10, length.out = 10)

grid <- expand.grid(x, y, stringsAsFactors = FALSE)
grid$z <- numeric(100)
for (i in 1:100) {
  grid$z[i] <- D_crit(infomat(grid[i, 1], grid[i, 2], 100, 1))
}

ggplot(grid) +
  geom_contour(data = grid, aes(x = Var1, y = Var2, z = as.double(z)),
               color = "black") +
  xlim(1, 10) +
  ylim(1, 10)

summary(grid$z)
boxplot(grid$z)
which.min(grid$z)
