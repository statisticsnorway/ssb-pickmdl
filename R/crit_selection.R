

#' Selection from table of PICKMDL criteria 
#'
#' @param crit_tab Output from  \code{\link{crit_table}}
#' @param pickmdl_method A replacement for the original `PICKMDL` argument, `method`. 
#'          Possible values are `"first"`(default) and `"aic"`. 
#'          The latter is an alternative to the original method, `"best"` (not implemented). 
#' @param star Index to be selected when no model is ok according to criteria. 
#' @param when_star Function to be called when no model is ok according to criteria. 
#'                  Supply `stop` to invoke error.  Supply `NULL` to do nothing.
#' @return Selected index
#' @export
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' spec5 <- x13_spec_pickmdl(spec = "RSA3", transform.function = "Log")
#' 
#' sa5 <- x13_multi(myseries, spec = spec5)   
#'
#' tab <- crit_table(sa5)
#' 
#' crit_selection(tab)
#' crit_selection(tab[2:5, ])
#' crit_selection(tab[1:2, ]) # Warning
#' crit_selection(tab[1:2, ], star = 2, when_star = message) 
#' crit_selection(tab[5:1, ])
#' crit_selection(tab[5:1, ], pickmdl_method = "aic")
#' 
crit_selection <- function(crit_tab, pickmdl_method = "first", star = 1, when_star = warning){
  if (star > nrow(crit_tab)) {
    stop("The value of star is too large")
  }
  if(!(pickmdl_method %in% c("first", "aic")))
    stop('Allowed values of parameter pickmdl_method are "first" and "aic")')
  
  if(pickmdl_method == "aic") {
    x <- crit_tab[,"m_aic"]
  } else {
    x <- rep(1, nrow(crit_tab))
  }
  x[!(crit_tab[,"crit1"] < 0.15 & crit_tab[,"crit2"] > 0.05 & crit_tab[,"crit3"] < 0.9)] <- Inf
  if (min(x) == Inf) {
    if (!is.null(when_star)) {
      when_star("No model is ok according to criteria")
    }
    return(star)
  }
  which.min(x)
}