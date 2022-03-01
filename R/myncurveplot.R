#' Title Normal Curve Area Plot
#'
#' @param mu the mean
#' @param sigma the standard deviation
#'
#' @return a normal curve plot showing the area of the probabilty based on
#' the x limits and the mean and standard deviation values
#' @export
#'
#' @examples
#' myncurve(5, 9)
myncurve = function(mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))}
