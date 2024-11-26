#' myncurve Function -> Displays a normal distribution curve with shaded probability P(Y <= a)
#'
#' @param mu -> Mean of normal distribution
#' @param sigma -> Standard deviation of normal distribution
#' @param a -> Shades probability of normal distribution P(Y <= a)
#'
#' @return A normal distribution curve with the shaded area and a list with mu, sigma, and probability
#' @export
#'
#' @importFrom stats dnorm pnorm
#' @importFrom graphics curve polygon
#'
#'@examples
#' myncurve(10, 10, 9)
myncurve = function(mu, sigma, a){
  # Build curve and area
  x = (mu - 3 * sigma):(mu + 3 * sigma)
  curve(dnorm(x = x,mean=mu, sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xpoints = seq(mu - 3 * sigma, a, length=1000)
  ypoints = dnorm(xpoints, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, xpoints, a), c(0, ypoints, 0), col="red")

  # Calculate Probability/Area and Return List
  area = round(pnorm(q = a, mean = mu, sd = sigma), 4)
  list(mu = mu, sigma = sigma, probability = area)
}
