
#' x13 with PICKMDL
#' 
#' \code{\link{x13}} is run with a PICKMDL specification
#' 
#'
#' @param series `x13` parameter
#' @param userdefined `x13` parameter
#' @param ... `x13_spec` parameters
#' @param arima.p `x13_spec` parameter as vector
#' @param arima.d `x13_spec` parameter as vector
#' @param arima.q `x13_spec` parameter as vector
#' @param arima.bp `x13_spec` parameter, possibly as vector
#' @param arima.bd `x13_spec` parameter, possibly as vector
#' @param arima.bq `x13_spec` parameter, possibly as vector
#' @param automdl.enabled `x13_spec` parameter
#'
#' @return An `x13` output object
#' @export
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' a <- x13_pickmdl(myseries, spec = "RSA3", transform.function = "Log")
#' a$regarima
#' 
#' allvar <- pickmdl_data("allvar")
#' 
#' b <- x13_pickmdl(myseries, spec = "RSA3", transform.function = "Log",
#'                  usrdef.varEnabled = TRUE, 
#'                  usrdef.varType = c("Calendar", "Calendar"), 
#'                  usrdef.var = allvar, 
#'                  outlier.enabled = FALSE, 
#'                  usrdef.outliersEnabled = TRUE,
#'                  usrdef.outliersType = rep("LS", 20), 
#'                  usrdef.outliersDate = c("2009-01-01", "2016-01-01", 
#'                                          "2020-03-01", "2020-04-01", "2020-05-01", 
#'                                          "2020-06-01", "2020-07-01", "2020-08-01", 
#'                                          "2020-09-01", "2020-10-01", "2020-11-01", 
#'                                          "2020-12-01", "2021-01-01", "2021-02-01",
#'                                          "2021-03-01", "2021-04-01", "2021-05-01",
#'                                          "2021-06-01", "2021-07-01", "2021-08-01"))
#' b$regarima
#' 
#' # Warning when transform.function = "None"
#' d <- x13_pickmdl(myseries, spec = "RSA3", transform.function = "None")
#'                                           
x13_pickmdl <- function(series, userdefined = NULL, ..., arima.p = c(0, 0, 2, 0, 2), 
                        arima.d = c(1, 1, 1, 2, 1), arima.q = c(1, 2, 0, 2, 2), 
                        arima.bp = 0, arima.bd = 1, arima.bq = 1,
                        automdl.enabled = FALSE) {
  
  n <- length(arima.p)
  
  if(length(arima.bp) == 1) arima.bp <- rep(arima.bp, n)
  if(length(arima.bd) == 1) arima.bd <- rep(arima.bd, n)
  if(length(arima.bq) == 1) arima.bq <- rep(arima.bq, n)
  
  spec_multi <- x13_spec_pickmdl(..., 
                               arima.p = arima.p, arima.d = arima.d, 
                               arima.q = arima.q, arima.bp = arima.bp, 
                               arima.bd = arima.bd, arima.bq = arima.bq,
                               automdl.enabled = automdl.enabled)
  
  sa_mult <- x13_multi(series = series, userdefined = userdefined, spec = spec_multi)
  
  crit_tab <- crit_table(sa_mult)
  
  mdl_nr <- crit_selection(crit_tab)
  
  # easy to return sa_mult[[mdl_nr]], but prepare for more general code
  # series for selection and final series for x13 may be different 
  
  x13(series = series, userdefined = userdefined, spec = spec_multi[[mdl_nr]])
  
}