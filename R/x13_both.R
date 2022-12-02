
#' \code{\link{x13_spec}}  and \code{\link{x13_pickmdl}} wrapped as a single function
#' 
#' Output is determined by the parameter `both_output`.
#' 
#' All parameters except `both_output` and `...` are parameters to \code{\link{x13_pickmdl}}.
#'
#' @inheritParams x13_pickmdl
#' @param ...  Parameters to \code{\link{x13_spec}}. 
#' @param userdefined  Parameter to \code{\link{x13}}  (via `...`  to `x13_pickmdl`).
#' @param both_output One of `"main"` (default, x13_pickmdl output), `"spec"` (spec output) or `"both"`. 
#'
#' @return By default an `x13` output object, or otherwise a list as specified by parameter `output` and `both_output`.
#' @export
#' 
#' @examples
#' a <- x13_both(pickmdl_data("myseries"), spec = "RSA3", transform.function = "Log", verbose = TRUE)
#' a$decomposition
#'
x13_both <- function(series, ..., userdefined = NULL, 
                     pickmdl_method = "first", star = 1, when_star = warning,
                     identification_end = NULL, identification_estimate.to = NULL, 
                     identify_t_filter = FALSE, identify_s_filter = FALSE, 
                     identify_outliers = FALSE,
                     automdl.enabled = FALSE,
                     verbose = FALSE,
                     output = "sa", 
                     both_output = "main") {
  
  if(!(both_output %in% c("main", "spec", "both")))
    stop('Allowed values of parameter both_output are "main", "spec" and "both".')
  
  spec <- x13_spec(...)
  
  if(both_output == "spec"){
    return(spec)
  }
  
  main  <- x13_pickmdl(series = series, spec = spec, userdefined = userdefined, 
                       pickmdl_method = pickmdl_method, star = star, when_star = when_star,
                       identification_end = identification_end, identification_estimate.to = identification_estimate.to, 
                       identify_t_filter = identify_t_filter, identify_s_filter = identify_s_filter, 
                       identify_outliers = identify_outliers,
                       automdl.enabled = automdl.enabled,
                       verbose = verbose,
                       output = output)
  
  
  if(both_output == "both"){
    return(list(main = main, spec = spec))
  }
  
  main
}