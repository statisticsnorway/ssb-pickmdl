update_spec_outliers <- function(spec, sa, day = "01") {
  
  if (frequency(sa$final$forecasts) != 12) {
    stop("Only frequency==12 implemented")
  }
  
  
  start_ <- start(sa$final$forecasts)
  from_ <- sub(".", "-", sprintf("%7.2f", (start_[1] + start_[2]/100)), fixed = TRUE)
  new_outlier.from <- paste(from_, day, sep = "-")
  
  
  s_span_ <- s_span(spec)
  old_outlier.from <- s_span_[rownames(s_span_) == "outlier", "d0"]
  
  if (new_outlier.from <= old_outlier.from) {
    cat("outlier.from not updated:", old_outlier.from, "\n")
    return(spec)
  }
  
  spec <- x13_spec(spec, outlier.from = new_outlier.from)
  
  cat("outlier.from updated:", new_outlier.from)
  
  updated <- update_outliers(spec = spec, sa = sa, day = day, null_when_no_new = TRUE)
  
  cat("\n")
  
  if (is.null(updated)) {
    return(spec)
  }
  
  x13_spec(spec, usrdef.outliersType = updated$type, usrdef.outliersDate = updated$date)
  
}


update_outliers <- function(spec, sa, day = "01", null_when_no_new = TRUE) {
  
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
  
  sa_o <- sa_o[!(sa_o$date %in% substr(pre$date, 1, 7)), , drop = FALSE]
  
  if (null_when_no_new & !nrow(sa_o)) {
    cat("  No new outliers.")
    return(NULL)
  }
  cat("  New outliers:", paste(sa_o$date, collapse = ", "))
  
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
    return(character(0))
  }
  k <- k[kis3]
  year <- as.integer(sapply(k, function(x) x[3]))
  month <- as.integer(sapply(k, function(x) x[2]))
  date_mnd <- sub(".", "-", sprintf("%7.2f", (year + month/100)), fixed = TRUE)
  
  type <- trimws(sapply(k, function(x) x[1]))
  
  data.frame(type = type, date = date_mnd)
  
}