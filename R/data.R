#' Balanced Designs data
#'
#' A catalog of balanced designs using the one-way ANOVA model
#'
#' @format ## `who`
#' A data frame with 96 rows and 7 columns:
#' \describe{
#'   \item{N}{total number of experiment runs}
#'   \item{a}{number of groups in the random effect}
#'   \item{n}{number of replicates per group}
#'   \item{tau}{the ratio of the random effect variance component and the
#'   residual error variance component}
#'   \item{Criteria}{a character indicating the use of the A or D criteria for
#'   the 'Score' columm}
#'   \item{Score}{the score of the criteria for that row's experiment design}
#'   \item{Cross.Score}{the score of the criteria not indicated in the
#'   'Criteria' column for that row's experiment design}
#'   ...
#' }
#'
"balanced_designs"
