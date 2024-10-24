
#' Table of PICKMDL criteria 
#' 
#' Function `crit_table` takes several `x13` output objects as input and produces a table of criteria (matrix class). 
#' The other functions are underlying functions that take a single `x13` output object as input. 
#'
#' @param sa_list List of several `x13` output objects. That is, `spec` can be output from  \code{\link{x13_multi}}.
#'
#' @return A matrix, a vector or a single numerical value 
#' @export
#' @importFrom utils tail
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' spec5 <- x13_spec_pickmdl(spec = "RSA3", transform.function = "Log")
#' 
#' sa5 <- x13_multi(myseries, spec = spec5)   
#'
#' crit_table(sa5)
#' 
#' crit123_m_aic(sa5[[4]])
#' 
#' crit1(sa5[[4]])
#' crit2(sa5[[4]])
#' crit3(sa5[[4]])
#' m_aic(sa5[[4]]) 
crit_table <- function(sa_list){
  t(sapply(sa_list, crit123_m_aic))
} 


#' @rdname crit_table
#' @param sa A single `x13` output object. 
#' @export
crit123_m_aic <- function(sa){
  c(crit1 = crit1(sa),
    crit2 = crit2(sa),
    crit3 = crit3(sa),
    m_aic = m_aic(sa)) 
}


#' @rdname crit_table
#' @export
crit1 <- function(sa){
  input_series <- sa$final$series[,"y"]
  freq <- frequency(input_series)
  rest_l     <- sa$regarima$residuals
  rest_l1    <- tail(rest_l, 3 * freq)
  resid1_pct <- 100 * rest_l1 / tail(input_series, 3 * freq)  ## data_inn1
  mean(abs(resid1_pct))
} 


#' @rdname crit_table
#' @export
crit2 <- function(sa){
  hj1 <- sa$regarima$residuals.stat$tests$P.value
  hj1[4] 
}


#' @rdname crit_table
#' @export
crit3 <- function(sa){
  hj2 <- sa$regarima$arima.coefficients
  hj3 <- hj2[substr(rownames(hj2), 1, 5) == "Theta", ]
  
  if (!is.matrix(hj3)) {
    return(as.numeric(hj3[1])) # as.numeric removes name (Estimate)
  } 
  sum(hj3[, 1])
}


#' @rdname crit_table
#' @export
m_aic <- function(sa){
  hj1 <- sa$regarima$loglik
  hj1["aic", ]
}

