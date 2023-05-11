
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
x13_both <- function(series, ..., userdefined = NULL, both_output = "main",  
                        corona = FALSE, 
                        pickmdl_method = "first", star = 1, 
                        when_star = warning,
                        when_automdl = message,
                        when_finalnotok = NULL,
                        identification_end = NULL, identification_estimate.to = NULL, 
                        identify_t_filter = FALSE, identify_s_filter = FALSE, 
                        identify_outliers = TRUE,
                        identify_arima_mu = TRUE,
                        automdl.enabled = FALSE,
                        fastfirst = TRUE,
                        verbose = FALSE,
                        output = "sa",
                        add_comment = TRUE){
  if(!(both_output %in% c("main", "spec", "both")))
    stop('Allowed values of parameter both_output are "main", "spec" and "both".')
  
  spec <- x13_spec(...)
  
  if(both_output == "spec"){
    return(spec)
  }
  
  # The function definition of x13_both is, for most parameters, made to be 
  # identical to  x13_pickmdl (same parameters and default values). 
  # This is also tested in the package test.  
  # Below: 
  #   old method is more understandable
  #   new method is safer in terms of updates of x13_pickmdl
  
  # Trick to run old_method for testing: 
  x13_both_old_method <- get0("x13_both_old_method", ifnotfound = FALSE)
  
  if(x13_both_old_method){
    message("x13_both_old_method = TRUE")
  main  <- x13_pickmdl(series = series, spec = spec, userdefined = userdefined, 
                       corona = corona, 
                       pickmdl_method = pickmdl_method, star = star, when_star = when_star,
                       when_automdl = when_automdl, when_finalnotok = when_finalnotok, 
                       identification_end = identification_end, identification_estimate.to = identification_estimate.to, 
                       identify_t_filter = identify_t_filter, identify_s_filter = identify_s_filter, 
                       identify_outliers = identify_outliers, identify_arima_mu = identify_arima_mu,
                       automdl.enabled = automdl.enabled,
                       fastfirst = fastfirst, verbose = verbose, 
                       output = output,  add_comment =  add_comment)
  } else {
    dot_names <- names(list(...))
    m_call <- match.call()
    m_call <- m_call[!(names(m_call) %in% c(dot_names, "both_output"))]
    m_call[["spec"]] <- spec
    m_call[[1]] <- x13_pickmdl
    main <- eval(m_call)
  }
                              
  if(both_output == "both"){
    return(list(main = main, spec = spec))
  }
  
  main
}