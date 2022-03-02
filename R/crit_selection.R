

#' Selection from table of PICKMDL criteria 
#'
#' @param crit_tab Output from  \code{\link{crit_table}}
#'
#' @return Selected index
#' @export
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' spec5 <- x13_spec_multi(spec = "RSA3", transform.function = "Log")
#' 
#' sa5 <- x13_multi(myseries, spec = spec5)   
#'
#' tab <- crit_table(sa5)
#' 
#' crit_selection(tab)
#' crit_selection(tab[2:5, ])
#' crit_selection(tab[1:2, ]) # Warning
#' 
crit_selection <- function(crit_tab){
  x <- crit_tab[,"m_aic"]
  x[!(crit_tab[,"crit1"] < 0.15 & crit_tab[,"crit2"] > 0.05 & crit_tab[,"crit3"] < 0.9)] <- Inf
  if (min(x) == Inf){
    warning("No model is ok according to criteria")
  }
  which.min(x)
}