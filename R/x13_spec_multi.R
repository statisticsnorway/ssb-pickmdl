
#' Multiple X-13ARIMA model specifications
#' 
#' \code{\link{x13_spec}} is run multiple times with input for multiple arima models.
#' 
#' This function behaves like `x13_spec` except that some of the parameters may be vectors.
#' These vectors must be the same length.
#'
#' @param ... `x13_spec` parameters
#' @param arima.p `x13_spec` parameter as vector
#' @param arima.d `x13_spec` parameter as vector
#' @param arima.q `x13_spec` parameter as vector
#' @param arima.bp `x13_spec` parameter, possibly as vector
#' @param arima.bd `x13_spec` parameter, possibly as vector
#' @param arima.bq `x13_spec` parameter, possibly as vector
#' @param automdl.enabled `x13_spec` parameter
#'
#' @return List of several `x13_spec` output objects
#' @export
#' @importFrom RJDemetra x13_spec
#'
#' @examples
#' spec5 <- x13_spec_multi(spec = "RSA3", transform.function = "Log")
#' 
x13_spec_multi <- function(..., arima.p = c(0, 0, 2, 0, 2), 
                             arima.d = c(1, 1, 1, 2, 1), arima.q = c(1, 2, 0, 2, 2), 
                             arima.bp = 0, arima.bd = 1, arima.bq = 1,
                             automdl.enabled = FALSE ) {
  n <- length(arima.p)
  
  if(length(arima.bp) == 1) arima.bp <- rep(arima.bp, n)
  if(length(arima.bd) == 1) arima.bd <- rep(arima.bd, n)
  if(length(arima.bq) == 1) arima.bq <- rep(arima.bq, n)
  
  
  if (length(arima.d) != n | length(arima.q) != n) {
    stop("arima.p, arima.d and arima.q must have same length")
  }
  
  if (length(arima.bp) != n | length(arima.bd) != n | length(arima.bq) != n) {
    stop("arima.bp, arima.bd and arima.bq must have length 1 or same length as other parameters")
  }
  
  
  spec <- vector("list", n)  
  for (i in 1:n) {
    spec[[i]] <- x13_spec(..., 
                          arima.p = arima.p[i], arima.d = arima.d[i], 
                          arima.q = arima.q[i], arima.bp = arima.bp[i], 
                          arima.bd = arima.bd[i], arima.bq = arima.bq[i],
                          automdl.enabled = automdl.enabled)
  }
  spec
}


