
#' PICKMDL "first" check
#' 
#' Check whether \code{\link{x13}} output is ok according to   
#' the  PICKMDL "first" method
#' 
#' Unlike \code{\link{ok}}, this function does the actual calculations.
#'
#' @param sa A \code{\link{x13}} output object
#'
#' @return `TRUE` or `FALSE`
#' @export
#' 
#' @seealso \code{\link{crit_selection}}
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' a <- x13(myseries, x13_spec(spec = "RSA3", transform.function = "Log"))
#' b <- x13(myseries, x13_spec(spec = "RSA3", transform.function = "None"))
#' 
#' crit_ok(a)
#' crit_ok(b)
#' 
crit_ok <- function(sa) {
  as.logical(crit_selection(crit_table(list(sa)), star = 0, when_star = NULL))
}
