#' place holder
#'
#' @param N place holder
#' @param a place holder
#' @param b place holder
#'
#' @return place holder
#' @export
#'

##################### Resources and Thoughts ###################################
# https://stackoverflow.com/questions/52480256/r-fill-a-list-of-lists-with-a-loop-produces-an-error
# https://stackoverflow.com/questions/46951980/self-written-r-package-does-not-find-its-own-function

# what if i start with all partitions of N, then shuffle those partitions,
# and randomly split them into groups, and repeat? If i iterate enough times
# it should be a pretty extensive group


generate_designs_3VC_U <- function(N, a, b, sig_a_sq, sig_b_sq, error_sq) {

  # Defaults for testing
  N = 8
  a = 3
  b = 2
  i = 2
  j=1
  k=3
  l = 1

  designs <- data.frame("N" = double(1),
                        "a" = double(1))

  # Create a_i vectors to denote number of sub-groups per level of alpha
  a_vecs <- data.frame(matrix(vector(mode = 'numeric',length = a * 1), nrow = 1, ncol = a))
  for (i in seq_len(a)) {
    names(a_vecs)[i] <- paste0("a_", i)
  }

  # create b_ij vectors to denote number of replications per sub-group of alpha
  b_vecs <- data.frame(matrix(vector(mode = 'numeric', length = a * b * 1), nrow = 1, ncol = a * b))
  count = 0
  for (i in seq_len(a)) {
    for (j in seq_len(b)) {
      count = count + 1
      names(b_vecs)[count] <- paste0("b_", i, j)
    }
  }

  # Add newly genereated columns and assign values for N and a
  designs <- cbind(designs, a_vecs, b_vecs)
  designs$N <- N
  designs$a <- a

  # Shows all combinations of number of sub-groups per level of alpha
  fill_a_vecs <- combinations(n = b, r = a, v = c(1:b), repeats.allowed = TRUE)

  # Total number of replicates per level of alpha
  parts_a <- as.matrix(restrictedparts(n = N, m = a, include.zero = FALSE), nrow = a) #getCols(parts(N), A = a, N = N)

  parts_a_list <- vector(mode = "list", length = ncol(parts_a))
  for (i in seq_len(ncol(parts_a))) {
    parts_a_list[[i]] <- parts_a[, i] #saves individual partitions of a level as vectors
  }

  ####### New Idea #######
  for (i in seq_len(nrow(fill_a_vecs))){
    for (j in seq_len(ncol(parts_a))) {
      if (all(fill_a_vecs[i, ] == 1)) {
        temp <- c(N, a, fill_a_vecs[i, ], parts_a[, j])
        designs <- rbind(designs, temp=temp[seq(ncol(designs))])
      } else {
        for (k in seq_len(a)) {
          if (fill_a_vecs[i, k] != 1 & parts_a[k,j] != 1) {
            parts_b <- as.matrix(
              restrictedparts(n = parts_a[k, j],
                              m = fill_a_vecs[i, k],
                              include.zero = FALSE), nrow = fill_a_vecs[i, k]) #getCols(P = parts(parts_a[k, j]), A = fill_a_vecs[i, k], N = parts_a[k, j])
            for (l in seq_along(parts_b)) {
              if (!is.null(ncol(parts_b))) {
                if (ncol(parts_b) > 1) {
                  temp <- c(N, a, fill_a_vecs[i, ], parts_b[, l])
                  designs <- rbind(designs, temp=temp[seq(ncol(designs))])
                }
              } else {
                temp <- c(N, a, fill_a_vecs[i, ], parts_b[l])
                designs <- rbind(designs, temp=temp[seq(ncol(designs))])
              }
            }
          }
        }
      }
    }
  }
}

  # # Number of Replicates within each sub-group b_ij
  # parts_b_list <- list()
  # count = 0
  # for(l in seq_len(ncol(parts_a))){
  #   parts_b_list[[l]] = list()
  #   for (i in parts_a_list) {
  #     for (j in seq_len(a)) {
  #       if (i[j] != 1) {
  #         elem_parts <- getCols(parts(i[j]), b, i[j])
  #         parts_b_list[[l]] <- c(parts_b_list[[l]], list(elem_parts))
  #
  #         # count = count + 1
  #         # print(count)
  #       }
  #     }
  #   }
  # }

#   ############### Scratch Code ##########################
#   parts_b_list <- parts_b_list[[1]]
#
#   for (k in seq_along(ncol(elem_parts))) {
#     #print("Check k")
#     temp <-  temp[-j]
#     temp <-  append(x = temp,
#                     values = unlist(elem_parts[,k]),
#                     after = j-1)
#     print(temp)
#     temp_list[[count]] <- temp
#   }
# }
# }
# }
#
# parts_b <- vector(mode = "list", length = ncol(parts_a))
# for (i in seq_len(nrow(parts_a))){
#   for (j in seq_len(ncol(parts_a))){
#     if (parts_a[i,j] != 1) {
#       parts_b[[j]][[i]] <- getCols(parts(parts_a[i,j]), A = b, N = parts_a[i,j])
#     }
#   }
# }
# }
#
#
#
# test_function <- function(N, a, b){
#
# }
#
# # Example that kind of works
#
# N = 8
# a = 3
# b = 2
# parts_a <- getCols(parts(N), A = a, N = N)
# parts_a_list <- vector(mode = "list", length = ncol(parts_a))
# for (i in seq_len(ncol(parts_a))) {
#   parts_a_list[[i]] <- parts_a[, i]
# }
#
# # parts_b[,i] = parts_a[,i] but everything that is not 1 is also partitioned
# # mostly works, something goes wrong in how the for loops iterate
# parts_b <- vector(mode = "list", length = ncol(parts_a))
# parts_b_list <- vector(mode = "list", length = 20)
# count = 0
# i=1
# j=1
# k=1
# for (i in seq_along(parts_a_list)){
#   count = count + 1
#   parts_b[[i]] = list(parts_a[,i])
#   for (j in seq_along(parts_b[[i]])) {
#     if (parts_b[[i]][[i]][j] != 1) {
#       k_len <- ncol(getCols(parts(parts_b[[i]][[i]][j]), b, parts_b[[i]][[i]][j]))
#       for (k in seq_len(k_len)) {
#         elem_parts <- getCols(parts(parts_b[[i]][[i]][j]), b, parts_b[[i]][[i]][j])
#         parts_b[[i]][[i]][j] <- list(elem_parts[, k])
#         parts_b_list[[count]] <- unlist(parts_b[[i]][[i]])
#       }
#     }
#   }
# }
