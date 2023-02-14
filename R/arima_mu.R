#' Capture `arima.mu` from x13 output 
#' 
#'
#' @param sa A \code{\link{x13}} output object
#'
#' @return `TRUE` or `FALSE`
#' @export
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' a <- x13(myseries, spec = "RSA3")
#' b <- x13(sqrt(myseries), spec = "RSA3")
#' 
#' arima_mu(a)
#' arima_mu(b)
#' 
arima_mu <- function(sa) {
  if (is.null(sa$regarima)) {
    stop("Wrong input: regarima not found.")
  }
  "Mean" %in% rownames(sa$regarima$regression.coefficients)
}