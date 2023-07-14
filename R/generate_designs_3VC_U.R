generate_designs_3VC_U <- function() {
  unbalanced_designs <- data.frame("N" = double(10000),
                                   "a" = double(10000),
                                   "n_i" = character(10000),
                                   "sig_a" = character(10000),
                                   "criteria" = character(10000),
                                   "score" = double(10000),
                                   "cross score" = double(10000),
                                   "relative efficiency" = double(10000))
}
N = 8
a = 3
reps <- getCols(parts(N), A = a, N = N)
reps <- reps[1:a, ]

reps2 <- vector(mode = "list", length = ncol(reps))

for (i in seq_len(nrow(reps))){
  for (j in seq_len(ncol(reps))){
    if (reps[i,j] != 1) {
      reps2[[j]][i] <- list(getCols(parts(reps[i,j]), A = 2, N = reps[i,j]))
    }
  }
}

reps
reps2

n_a <- character(100)
reps3 <- numeric(100)
for(i in seq_along(reps2)){
  reps3[i] <- length(reps2[[i]])
}
reps3 <- reps3[reps3 != 0]
count = 0
for (i in seq_along(ncol(reps))) {
  for (j in reps3) {
    count = count + 1
    n_a[count] <- reps2[[j]][i]
  }
}


designs <- data.frame("N" = 8,
                      "n_a" =)
