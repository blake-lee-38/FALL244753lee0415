#' My Summary Function
#'
#' @param x -> A regression model object
#'
#' @return A formatted summary of the object with the data we want
#' @export
#'
mysummary <- function(x){
  info <- summary(x)
  paste("Multiple R^2:", info$r.squared, "Adjusted R^2:", info$adj.r.squared)
}
