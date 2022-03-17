#' Update x13 spec with outliers 
#' 
#' Update an `x13_spec` output object with outliers from an `x13` output object.
#'
#' @param spec An \code{\link{x13_spec}} output object 
#' @param sa   An \code{\link{x13}} output object
#' @param day Day of month as character to be used in outlier coding 
#' @param verbose Printing information to console when `TRUE`.
#'
#' @return `update_spec_outliers` returns an updated `x13_spec` output object with
#'          new outliers and updated `outlier.from`.  
#'         `update_outliers` returns a data frame with outlier variables used to update. 
#' @export
#' @importFrom stats end frequency
#' @importFrom RJDemetra s_span s_preOut
#'
#' @examples
#' myseries <- ipi_c_eu[, "FR"]
#' 
#' spec_1 <- x13_spec(spec = "RSA3", transform.function = "None", usrdef.outliersEnabled = TRUE, 
#'                    usrdef.outliersType = "AO", usrdef.outliersDate = "2007-03-01", 
#'                    outlier.usedefcv = FALSE, outlier.cv = 3)
#'                    
#' spec_2 <- x13_spec(spec_1, estimate.to = "2018-08-01")
#' 
#' a <- x13(myseries, spec_2)
#' 
#' update_outliers(spec_1, a)
#' 
#' spec_3 <- update_spec_outliers(spec_1, a)
#' 
#' s_span(spec_1)
#' s_span(spec_2)
#' s_span(spec_3)
#' 
#' s_preOut(spec_1)
#' s_preOut(spec_2)
#' s_preOut(spec_3)
update_spec_outliers <- function(spec, sa, day = "01", verbose = FALSE) {
  
  freq = frequency(sa$final$series)
  
  if (freq  != 12) {
    stop("Only frequency==12 implemented")
  }
  
  # sa$regarima$model$spec_rslt$T.span is "dangerous" hack
  # but general solution (s_span(sa) is not and end of series is not)
  end_span = strsplit(sa$regarima$model$spec_rslt$T.span, split="to ")[[1]][2]
  
  end_span_integer = rev(as.integer(strsplit(end_span, split = "-")[[1]]))
  
  new_from_integer = end(ts(1:2, start = end_span_integer, frequency = freq))
  
  from_ <- sub(".", "-", sprintf("%7.2f", (new_from_integer[1] + new_from_integer[2]/100)), fixed = TRUE)
  new_outlier.from <- paste(from_, day, sep = "-")
  
  
  s_span_ <- s_span(spec)
  old_outlier.from <- s_span_[rownames(s_span_) == "outlier", "d0"]
  
  if (new_outlier.from <= old_outlier.from) {
    if(verbose) cat("outlier.from not updated:", old_outlier.from, "\n")
    return(spec)
  }
  
  spec <- x13_spec(spec, outlier.from = new_outlier.from)
  
  if(verbose) cat("outlier.from updated:", new_outlier.from)
  
  updated <- update_outliers(spec = spec, sa = sa, day = day, null_when_no_new = TRUE, verbose = verbose)
  
  
  if (is.null(updated)) {
    return(spec)
  }
  
  x13_spec(spec, usrdef.outliersEnabled = TRUE, usrdef.outliersType = updated$type, usrdef.outliersDate = updated$date)
  
}

#' @rdname update_spec_outliers
#' @param null_when_no_new Whether to return `NULL` when no new outliers found. 
#' @export
update_outliers <- function(spec, sa, day = "01", null_when_no_new = TRUE, verbose = FALSE) {
  
  pre <- s_preOut(spec)
  
  if (!length(nrow(pre))) {
    pre <- matrix(0, 0, 0)  # nrow is 0
  }
  
  if (!nrow(pre)) {  # when nrow is 0
    pre <- data.frame(type = character(0), date = character(0))
  } else {
    pre <- pre[, c("type", "date")]
  }
  
  pre_date_mnd <- substr(pre$date, 1, 7)
  
  sa_o <- sa_out(sa)
  
  if (length(sa_o)) {
    sa_o <- sa_o[!(sa_o$date %in% substr(pre$date, 1, 7)), , drop = FALSE]
  } else {
    sa_o <- matrix(0, 0, 0)  # nrow is 0
  }
  
  if (null_when_no_new & !nrow(sa_o)) {
    if(verbose) cat("  No new outliers.\n")
    return(NULL)
  }
  if(verbose) cat("  New outliers:", paste(sa_o$date, collapse = ", "), "\n")
  
  sa_o$date <- paste(sa_o$date, day, sep = "-")
  
  rbind(pre, sa_o)
  
}


sa_out <- function(a) {
  
  s <- row.names(a$regarima$regression.coefficients)
  if (!length(s)) {
    return(character(0))
  }
  
  k <- strsplit(s, split = "[()-]")
  
  kis3 <- (sapply(k, length) == 3 & grepl("(", s, fixed = TRUE))
  
  if (!sum(kis3)) {
    return(data.frame(type = character(0), date = character(0)))
  }
  k <- k[kis3]
  year <- as.integer(sapply(k, function(x) x[3]))
  month <- as.integer(sapply(k, function(x) x[2]))
  date_mnd <- sub(".", "-", sprintf("%7.2f", (year + month/100)), fixed = TRUE)
  
  type <- trimws(sapply(k, function(x) x[1]))
  
  data.frame(type = type, date = date_mnd)
  
}










