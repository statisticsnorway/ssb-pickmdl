#' Multiple x13 runs from multiple specifications
#' 
#' \code{\link{x13}} is run multiple times 
#' 
#' This function behaves like `x13` except that parameter `spec` is a list of multiple specifications.
#'
#' @param ... `x13` parameters
#' @param spec List of several `x13_spec` output objects. That is, `spec` can be output from  \code{\link{x13_spec_multi}}.
#'
#' @return List of several `x13` output objects
#' @export
#' @importFrom RJDemetra x13
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' spec5 <- x13_spec_multi(spec = "RSA3", transform.function = "Log")
#' 
#' sa5 <- x13_multi(myseries, spec = spec5)   
#'            
x13_multi <- function(..., spec) {
  n <- length(spec)
  sa <- vector("list", n)
  for (i in 1:length(spec)) {
    sa[[i]] <- x13(..., spec = spec[[i]])
  }
  sa
}