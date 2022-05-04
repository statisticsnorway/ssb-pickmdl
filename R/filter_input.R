

#' x13 output filters to x13 input filters
#' 
#' Elements `t_filter` and `s_filter` are transformed to input parameters `x11.trendma` and `x11.seasonalma` 
#' 
#'
#' @param sa A \code{\link{x13}} output object
#'
#' @return list of `x11.trendma` (numeric) and `x11.seasonalma` (character)
#' @export
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' a <- x13(myseries, spec = "RSA3")
#' 
#' a$decomposition$t_filter
#' a$decomposition$s_filter
#' filter_input(a)
#' 
#' spec_b <- x13_spec(spec = "RSA3", x11.trendma = 13, x11.seasonalma = "Stable", 
#'                    x11.trendAuto = FALSE)
#' b <- x13(myseries, spec = spec_b)
#' 
#' b$decomposition$t_filter
#' b$decomposition$s_filter
#' filter_input(b)
filter_input <- function(sa) {
  x11.trendma <- as.numeric(split_for_filter(sa$decomposition$t_filter))
  x11.seasonalma <- split_for_filter(sa$decomposition$s_filter)
  if (is.na(x11.trendma)) {
    stop("Could not find x11.trendma")
  }
  if (x11.trendma < 1) {
    stop("Could not find correct x11.trendma")
  }
  if (substr(x11.seasonalma, 2, 2) %in% c("x", "X")) {
    x11.seasonalma <- paste0("S", x11.seasonalma)
    x11.seasonalma <- toupper(x11.seasonalma)
  }
  list(x11.trendma = x11.trendma, x11.seasonalma = x11.seasonalma)
}

split_for_filter = function(s){
 unlist(strsplit(as.character(s), split = "[ -]"))[1]
}

