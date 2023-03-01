
#' Corona outliers
#' 
#' Corona outliers as a type-date data frame. Also a function to update spec with these outliers.
#' 
#' Corona outliers with same date as outliers already in spec will be omitted.   
#'
#' @param option Only `"ssb"` implemented
#' @param freq frequency, `4` or `12`
#' @param day day of month as character
#' @param q_month month of quarter as `1`, `2` or `3`
#' @param spec An \code{\link{x13_spec}} output object to be updated
#' @param outlier_date_limit  Only outliers with `date < outlier_date_limit` will be included in updated spec. 
#'
#' @return data frame
#' @export
#' 
#' @seealso \code{\link{s_preOut}}
#'
#' @examples
#' corona_outliers()
#' corona_outliers(freq = 4)
#' 
#' spec_a <- x13_spec(spec = "RSA3", transform.function = "Log")
#' spec_a2 <- update_spec_corona_outliers(spec_a)
#' s_preOut(spec_a)
#' s_preOut(spec_a2)
#' 
#' spec_b <- x13_spec(spec = "RSA3", transform.function = "Log", 
#'                    usrdef.outliersEnabled = TRUE, 
#'                    usrdef.outliersType = rep("AO", 3), 
#'                    usrdef.outliersDate = c("2009-01-01", "2016-01-01", "2020-05-01"))
#' spec_b2 <- update_spec_corona_outliers(spec_b, outlier_date_limit = "2021-11-01")
#' s_preOut(spec_b)
#' s_preOut(spec_b2)
corona_outliers <- function(option = "ssb", freq = 12, day = "01", q_month = 1) {
  if (option != "ssb") {
    stop('Only type "ssb" implemented')
  }
  if (!(freq %in% c(4, 12))) {
    stop("Only freq 4 and 12 implemented")
  }
  if (!(q_month %in% 1:3)) {
    stop("q_month must be in 1:3")
  }
  year <- rep(2020:2022, each = 12)
  month <- rep(1:12, 3)
  dates <- paste(year, Number(month, 2), day, sep = "-")
  
  if (freq == 4) {
    dates <- dates[q_month + 3 * (0:8)]
  }
  if (freq == 12) {
    dates <- dates[3:27]
  }
  data.frame(type = "TC", date = dates, stringsAsFactors = FALSE)
}


#' @rdname corona_outliers
#' @export
update_spec_corona_outliers <- function(spec, option = "ssb", freq = 12, day = "01", q_month = 1, outlier_date_limit = "3000-01-01") {
  co <- corona_outliers(option = option, freq = freq, day = day, q_month = q_month)
  co <- co[co$date < outlier_date_limit, , drop = FALSE]
  if (nrow(co)) {
    updated <- update_outliers(sa = co, spec = spec, day = day)  # as.character for old r versions
  } else {
    updated <- NULL
  }
  if (is.null(updated)) {
    return(spec)
  }
  x13_spec(spec, usrdef.outliersEnabled = TRUE, usrdef.outliersType = as.character(updated$type), usrdef.outliersDate = as.character(updated$date))
} 



# SSBtools::Number
Number <- function(n, width = 3) {
  s <- "s <- sprintf('%0d', n)"
  s <- gsub("0", as.character(width), s)
  eval(parse(text = s))
  s <- gsub(" ", "0", s)
  s
}