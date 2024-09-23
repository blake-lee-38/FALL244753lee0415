#' The Get Probs Function (Emulates Binomial Experiment)
#'
#' @param min -> minimum number of successes
#' @param max -> maximum number of successes
#' @param size -> number of trials in experiment
#' @param p -> probability of a success
#'
#' @return A Table of each possible number of successes between min and max and their distribution using dbinom
#' @export
#'
#' @examples
#' getProbs(0, 10, 10, 0.5)
getProbs <- function(min = 0, max = 10, size = 1, p = 0.5) {
  numSuccess <- min:max
  probs <- dbinom(numSuccess, size = size, prob = p)
  names(probs) <- numSuccess
  return(probs)
}
