
#' x13 with PICKMDL
#' 
#' \code{\link{x13}} is run with a PICKMDL specification
#' 
#' @param series `x13` parameter
#' @param spec List of several `x13_spec` output objects. That is, `spec` can be output from  \code{\link{x13_spec_pickmdl}}.
#' @param ... Further `x13` parameters (currently only parameter `userdefined` is additional parameter to `x13`).
#'
#' @return An `x13` output object
#' @export
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' spec_a  <- x13_spec_pickmdl(spec = "RSA3", transform.function = "Log")
#' 
#' a <- x13_pickmdl(myseries, spec_a)
#' a$regarima
#' 
#' allvar <- pickmdl_data("allvar")
#' 
#' spec_b <- x13_spec_pickmdl(
#'             spec = "RSA3", transform.function = "Log",
#'             usrdef.varEnabled = TRUE, 
#'             usrdef.varType = c("Calendar", "Calendar"), 
#'             usrdef.var = allvar, 
#'             outlier.enabled = FALSE, 
#'             usrdef.outliersEnabled = TRUE,
#'             usrdef.outliersType = rep("LS", 20), 
#'             usrdef.outliersDate = c("2009-01-01", "2016-01-01", 
#'                                     "2020-03-01", "2020-04-01", "2020-05-01", 
#'                                     "2020-06-01", "2020-07-01", "2020-08-01", 
#'                                     "2020-09-01", "2020-10-01", "2020-11-01", 
#'                                     "2020-12-01", "2021-01-01", "2021-02-01",
#'                                     "2021-03-01", "2021-04-01", "2021-05-01",
#'                                     "2021-06-01", "2021-07-01", "2021-08-01"))
#' b <- x13_pickmdl(myseries, spec_b)                                     
#' b$regarima
#' 
#' # Warning when transform.function = "None"
#' spec_d  <- x13_spec_pickmdl(spec = "RSA3", transform.function = "None")
#' d <- x13_pickmdl(myseries, spec_d) 
#'                                           
x13_pickmdl <- function(series, spec, ...) {
  
  if (!all(apply(sapply(spec, class),1, unique) == c("SA_spec", "X13"))) {
    stop("`spec` must be a list of `x13_spec` output objects. Run `x13_spec_pickmdl`?")
  }
  
  sa_mult <- x13_multi(series = series, spec = spec, ...)
  
  crit_tab <- crit_table(sa_mult)
  
  mdl_nr <- crit_selection(crit_tab)
  
  # easy to return sa_mult[[mdl_nr]], but prepare for more general code
  # series for selection and final series for x13 may be different 
  
  x13(series = series, spec = spec[[mdl_nr]], ...)
}

