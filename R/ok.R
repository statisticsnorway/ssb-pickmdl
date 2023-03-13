#' PICKMDL information as a list 
#' 
#' Extract `ok`, `ok_final` and `mdl_nr`  
#' 
#' @param sa Output from \code{\link{x13_pickmdl}} 
#'
#' @return List constructed from comment attribute
#' @export
#' 
#' @seealso \code{\link{crit_ok}}
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' a <- x13_pickmdl(myseries, x13_spec(spec = "RSA3", transform.function = "Log"))
#' b <- x13_pickmdl(myseries, x13_spec(spec = "RSA3", transform.function = "None"))
#' 
#' comment(a)
#' comment(b)
#' ok(a)
#' ok(b)
#' 
ok <- function(sa) {
  if(is.null(comment(sa))){
    warning("comment attribute missing")
    return(list(ok = NA, ok_final = NA, mdl_nr = NA_integer_))
  }
  a <- as.list(comment(sa))
  a$ok <- as.logical(a$ok)
  a$ok_final  <- as.logical(a$ok_final)
  a$mdl_nr <- as.integer(a$mdl_nr)
  a
}
