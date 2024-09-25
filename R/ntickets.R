#' ntickets Function -> Used to solve the plane overbooking problem
#'
#' @param N -> Number of seats on the plane
#' @param gamma -> Probability of overbooking
#' @param p -> Probability a given passenger will show up to the flight
#'
#' @return A named list of both the discrete and continuous results for the optimals number of tickets sold, as well as 2 plots showing the objective vs. n for both the discrete and continuous cases
#' @export
#'
#' @importFrom stats dbinom optimize pnorm qbinom qnorm
#' @importFrom graphics abline
#'
#'@examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.5) {
  # Step 1: Calculate bounds for testing/optimization
  mean <- N * p
  stdev <- sqrt(N * p * (1 - p))

  # Use N - stdev solely for readability to show "N" in the graph, use N + 3SD as upper
  # to encompass 99% of potential tickets sold values
  lowerBound <- floor(N - stdev)
  upperBound <- ceiling(N + 3 * stdev)
  interval <- lowerBound:upperBound


  # Step 2: Calculate number of tickets sold (Use Discrete)
  # Define discreteF function to use for optimization based on solely the discrete distribution
  discreteF = function(N = 200, p = 0.95, gamma = 0.02, n = 200){
    abs(N - qbinom(1 - gamma, n, p))
  }

  # Use optimize to find optimal tickets sold n, then, because optimize returns a decimal value, use floor and ceiling to test if rounding up or down would be most optimal (Lowest discreteF value)
  bestDN <- optimize(f = discreteF, interval = interval, N = N, p = p, gamma = gamma)
  bestDNLower = floor(bestDN$minimum)
  bestDNHigher = ceiling(bestDN$minimum)
  if (discreteF(N, p, gamma, bestDNLower) < discreteF(N, p, gamma, bestDNHigher)) {
    nd <- bestDNLower
  } else {
    nd <- bestDNHigher
  }

  # Step 3: Calculate Number of Tickets Sold (Using Continuous)
  # Define contF function to use for optimization based on the normal approximation
  contF = function(N = 200, p = 0.95, gamma = 0.02, n = 200){
    abs(N - qnorm(1 - gamma, n * p, sqrt(n * p * (1 - p))) + 0.5)
  }

  # Use optimize to find optimal tickets n (No need to round to integer)
  bestCN <- optimize(f = contF, interval = interval, N = N, p = p, gamma = gamma)
  nc <- round(bestCN$minimum, digits = 4)


  # Step 4: Create Named List of Results (Returned after plots generated)
  namedList <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  # Step 5: Create Plots (For discrete and continuous)
  # Define objective function to serve as y-values in plot
  objective <- function(n = 200, N = 200, p = 0.95, gamma = 0.02){
    mean <- n * p
    stdev <- sqrt(n * p * (1 - p))
    return(1 - gamma - pnorm(N, mean, stdev))
  }

  # Generate y-values using objective function and interval as x-values
  objective_results <- sapply(interval, objective, N = N, p = p, gamma = gamma)

  # Plot Discrete Objective Values
  plot(interval, objective_results, type="b", col="blue", main = paste("Objective vs n to find optimal tickets sold \n(", nd, ") gamma = ", gamma, " N = ", N, " Discrete"), ylab = "Objective Result", xlab = "n (Tickets Sold)")
  # Add a vertical line at the discrete result nd and a horizontal line for the objective value at nd
  abline(h=objective(nd, N, p, gamma), col="red")
  abline(v = nd, col = "red")

  # Plot Continuous Objective Values
  plot(interval, objective_results, type="l", col="blue", main = paste("Objective vs n to find optimal tickets sold \n(", nc, ") gamma = ", gamma, " N = ", N, " Continuous"), ylab = "Objective Result", xlab = "n (Tickets Sold)")
  # Add a vertical line at continuous result nc and horizontal line for objective value at nc
  abline(h=objective(nc, N, p, gamma), col="red")
  abline(v = nc, col = "red")


  return(namedList)
}
