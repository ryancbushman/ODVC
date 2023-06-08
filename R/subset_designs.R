

subset_designs <- function(N, dataset) {
  if (dataset == "balanced") {
    designs <- balanced_designs
  } else {
    designs <- unbalanced_designs
  }

  return(designs[which(designs$N == 100)])

}
