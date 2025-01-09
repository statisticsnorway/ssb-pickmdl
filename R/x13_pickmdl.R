
#' x13 with PICKMDL and partial concurrent possibilities  
#' 
#' \code{\link{x13}} can be run as usual (automdl) or with a PICKMDL specification.
#' The ARIMA model, outliers and filters can be identified at a certain date and then held fixed (with a new outlier-span).
#' 
#' @param series `x13` parameter
#' @param spec An \code{\link{x13_spec}} output object or a list of several objects as outputted from \code{\link{x13_spec_pickmdl}}. 
#'             In the case of a single object and when `automdl.enabled` is `FALSE`, `spec` will be converted internally 
#'             by `x13_spec_pickmdl` with default five arima model specifications. 
#' @param corona Whether to update `spec` by outliers according to \code{\link{corona_outliers}}.  
#'               `FALSE` or `NULL` means no update. `TRUE` or `"ssb"` means update.         
#' @param ... Further `x13` parameters (currently only parameter `userdefined` is additional parameter to `x13`).
#' @param pickmdl_method \code{\link{crit_selection}} parameter 
#'         or one of the two extra possibilities, `"first_automdl"` or `"first_tryautomdl"`.
#'         In both cases the `crit_selection` parameter is `"first"` and the automdl model is added as the last pickmdl model.
#' * **`"first_automdl"`:** The automdl model is chosen whenever no pickmdl model is ok.    
#'                          In other words, the `star` parameter changes.  
#' * **`"first_tryautomdl"`:** When no pickmdl model is ok:  The automdl model is chosen if this model is ok, 
#'                          otherwise the `star` model is chosen.  
#' @param star           \code{\link{crit_selection}} parameter
#' @param when_star      \code{\link{crit_selection}} parameter
#' @param when_automdl Function to be called when automdl since no pickmdl model ok. Supply NULL to do nothing.
#' @param when_finalnotok Function to be called, e.g. \code{\link{warning}}, when final run with final model is not ok. Supply NULL to do nothing.
#'                        See \code{\link{crit_ok}}. 
#' @param identification_end To shorten the series before runs used to identify (arima) parameters.
#'            That is, the series is shortened by `window(series,` `end = identification_end)`.
#' @param identification_estimate.to   To set \code{\link{x13_spec}} parameter `estimate.to` before runs used to identify (arima) parameters.  
#'            This is an alternative to  `identification_end`.
#' @param identify_t_filter When `TRUE`, Henderson trend filter is identified by the shortened (see above) series.
#' @param identify_s_filter When `TRUE`, Seasonal moving average filter is identified by the shortened series.
#' @param identify_outliers When `TRUE`, Outliers are identified by the shortened series.
#' @param identify_arima_mu When `TRUE`, `arima.mu` is identified by the shortened series (see \code{\link{arima_mu}}).
#' @param automdl.enabled When `TRUE`, automdl is performed instead of pickmdl. 
#'            If `spec` is a list of several objects as outputted from `x13_spec_pickmdl`, only first object is used.
#' @param fastfirst When `TRUE` and when pickmdl with `crit_selection` parameter `"first"`, 
#'                  only as many models as needed are run.
#'                  This affects the output when `output = "all"`. 
#' @param verbose Printing information to console when `TRUE`. 
#' @param output One of `"sa"` (default), `"spec"` (final spec), `"sa_spec"` (both) and `"all"`. See examples.   
#' @param add_comment When `TRUE`, a  comment attribute 
#'      (character vector with `ok`, `ok_final` and `mdl_nr`) will 
#'      be added to the \code{\link{x13}} output object. Use \code{\link{comment}} 
#'      to get the attribute or \code{\link{ok}} to get the attribute converted to a list. 
#' @param old_crit2  Logical. The p-value criterion used for PICKMDL criterion number 2.
#'       Set to `FALSE` for "Ljung-Box" and to `TRUE` for "Ljung-Box (residuals at seasonal lags)".
#'       This parameter can be overridden by setting the `"pickmdl.old_crit2"` option,
#'       in which case the option value will take precedence.
#'       The default value (`NA`) means that `old_crit2 = (date_found < "2024-10-15")`, 
#'       where `date_found` refers to the end date of the series used for model selection. 
#'       This date is determined by `identification_end` or `identification_estimate.to` when one of these is specified.
#'       The default value is chosen to ensure that the new criterion is phased in automatically. 
#'
#' @return By default an `x13` output object, or otherwise a list as specified by parameter `output`.
#' @export
#' @importFrom stats window
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' 
#' spec_a  <- x13_spec(spec = "RSA3", transform.function = "Log")
#' 
#' a <- x13_pickmdl(myseries, spec_a, verbose = TRUE)
#' comment(a)
#' ok(a)
#' unlist(ok(a))
#' a$regarima
#' 
#' a2 <- x13_pickmdl(myseries, spec_a, identification_end = c(2014, 2))
#' a2$regarima
#' 
#' # As above, another way
#' a3 <- x13_pickmdl(myseries, spec_a, identification_estimate.to = "2014-03-01")
#' a3$regarima
#' 
#' a4 <- x13_automdl(myseries, spec_a, identification_end = c(2014, 2))
#' a4$regarima
#' 
#' # As above, another way
#' spec_a_single  <- x13_spec(spec = "RSA3", transform.function = "Log")
#' a5 <- x13_automdl(myseries, spec_a_single, identification_estimate.to = "2014-03-01")
#' a5$regarima
#' 
#' allvar <- pickmdl_data("allvar")
#' 
#' spec_b <- x13_spec(
#'             spec = "RSA3", transform.function = "Log",
#'             usrdef.varEnabled = TRUE, 
#'             usrdef.varType = c("Calendar", "Calendar"), 
#'             usrdef.var = allvar, 
#'             outlier.enabled = FALSE, 
#'             usrdef.outliersEnabled = TRUE,
#'             usrdef.outliersType = rep("LS", 20), 
#'             usrdef.outliersDate = c("2009-01-01", "2016-01-01", 
#'                                     "2020-03-01", "2020-04-01", "2020-05-01", 
#'                                     "2020-06-01", "2020-07-01", "2020-08-01", 
#'                                     "2020-09-01", "2020-10-01", "2020-11-01", 
#'                                     "2020-12-01", "2021-01-01", "2021-02-01",
#'                                     "2021-03-01", "2021-04-01", "2021-05-01",
#'                                     "2021-06-01", "2021-07-01", "2021-08-01"))
#' b <- x13_pickmdl(myseries, spec_b, identification_end = c(2020, 2))                                     
#' b$regarima
#' 
#' # automdl instead  
#' b1 <- x13_automdl(myseries, spec_b, identification_end = c(2020, 2))
#' b1$regarima
#' 
#' # effect of identify_t_filter and identify_s_filter
#' set.seed(1)
#' rndseries <- ts(rep(1:12, 20) + (1 + (1:240)/20) * runif(240) + 0.5 * c(rep(1, 120), (1:120)^2), 
#'                 frequency = 12, start = c(2000, 1))
#' spec_c <- x13_spec(outlier.enabled = FALSE)               
#' c1 <- x13_automdl(rndseries, spec_c, identification_end = c(2009, 12))    
#' c1$decomposition
#' c2 <- x13_automdl(rndseries, spec_c, identification_end = c(2009, 12), identify_t_filter = TRUE) 
#' c2$decomposition
#' c3 <- x13_automdl(rndseries, spec_c, identification_end = c(2009, 12), identify_t_filter = TRUE, 
#'                   identify_s_filter = TRUE)     
#' c3$decomposition                       
#' 
#' 
#' # Warning when transform.function = "None"
#' spec_d  <- x13_spec(spec = "RSA3", transform.function = "None")
#' d <- x13_pickmdl(myseries, spec_d, verbose = TRUE)
#' 
#' # Warning avoided (when_star) and 2nd (star) model selected 
#' d2 <- x13_pickmdl(myseries, spec_d, star = 2, when_star = NULL, verbose = TRUE)
#' 
#' # automdl since no pickmdl model ok, but still not ok 
#' d3 <- x13_pickmdl(myseries, spec_d, pickmdl_method = "first_automdl", verbose = TRUE)
#' 
#' # airline model (star) since automdl also not ok 
#' d4 <- x13_pickmdl(myseries, spec_d, pickmdl_method = "first_tryautomdl", verbose = TRUE,
#'                   when_finalnotok = warning) # also finalnotok warning
#' 
#' # As a2, with output = "all"
#' k <- x13_pickmdl(myseries, spec_b, identification_end = c(2010, 2), output = "all",
#'                  fastfirst = FALSE) # With TRUE only one model in this case 
#' k$sa$decomposition  # As a2$decomposition 
#' k$mdl_nr            # index of selected model used to identify parameters
#' k$sa_mult[[k$mdl_nr]]$decomposition  # decomposition for model to identify
#' k$crit_tab          # Table of criteria 
#' 
#' 
#' # Effect of identify_outliers (TRUE is default)
#' m1 <- x13_pickmdl(myseries, x13_spec("RSA3", outlier.usedefcv = FALSE, outlier.cv = 3), 
#'                   identification_end = c(2010, 2), identify_outliers = FALSE)
#' m2 <- x13_pickmdl(myseries, x13_spec("RSA3", outlier.usedefcv = FALSE, outlier.cv = 3), 
#'                   identification_end = c(2010, 2), identify_outliers = TRUE, 
#'                   verbose = TRUE, output = "all")
#' m3 <- x13_pickmdl(myseries, m2$spec, identification_end = c(2018, 2), identify_outliers = TRUE, 
#'                   verbose = TRUE)
#' 
#' m1$regarima
#' m2$sa$regarima
#' m3$regarima
#' 
#' 
#' # With corona outliers (even possible when series is not long enough) 
#' m4 <- x13_pickmdl(myseries, spec_a, verbose = TRUE, corona = TRUE)
#' m4$regarima
#' m5 <- x13_pickmdl(myseries, x13_spec("RSA3", outlier.usedefcv = FALSE, outlier.cv = 3), 
#'                   identification_end = c(2010, 2), identify_outliers = TRUE, 
#'                   verbose = TRUE, corona = TRUE) 
#' m5$regarima 
#' 
#'  
#' ###########  quarterly series  #############
#'    
#' qseries <- pickmdl_data("qseries")    
#' 
#' # Effect of identify_outliers (TRUE is default)
#' q1 <- x13_pickmdl(qseries, x13_spec("RSA3", outlier.usedefcv = FALSE, outlier.cv = 3), 
#'                   identification_end = c(2010, 2), identify_outliers = FALSE)
#' q2 <- x13_pickmdl(qseries, x13_spec("RSA3", outlier.usedefcv = FALSE, outlier.cv = 3), 
#'                   identification_end = c(2010, 2), identify_outliers = TRUE, 
#'                   verbose = TRUE, output = "all")
#' q3 <- x13_pickmdl(qseries, q2$spec, identification_end = c(2018, 2), identify_outliers = TRUE, 
#'                   verbose = TRUE)
#' 
#' q1$regarima
#' q2$sa$regarima
#' q3$regarima
#' 
#' 
#' # With corona outliers (even possible when series is not long enough) 
#' q4 <- x13_pickmdl(qseries, spec_a, verbose = TRUE, corona = TRUE)
#' q4$regarima
#' q5 <- x13_pickmdl(qseries, x13_spec("RSA3", outlier.usedefcv = FALSE, outlier.cv = 3), 
#'                   identification_end = c(2010, 2), identify_outliers = TRUE, 
#'                   verbose = TRUE, corona = TRUE) 
#' q5$regarima 
#' 
#' 
#' # Demonstrate strange behavior of x13 with TC at end. Updating outlier.from matters.
#' # Explains why TC (III-2021) is in q4 and not in q5 
#' q11 <- x13(window(qseries, end = c(2020, 1)), 
#'     spec = x13_spec(spec = "RSA3", transform.function = "Log", 
#'     usrdef.outliersEnabled = TRUE, usrdef.outliersType = "TC", 
#'     usrdef.outliersDate = "2020-01-01"))  
#' q12 <- x13(window(qseries, end = c(2020, 1)), # same with outlier.from 
#'     spec = x13_spec(spec = "RSA3", transform.function = "Log", 
#'     usrdef.outliersEnabled = TRUE, usrdef.outliersType = "TC", 
#'     usrdef.outliersDate = "2020-01-01", outlier.from = "2020-04-01")) 
#' q11$regarima 
#' q12$regarima
#' 
#' 
x13_pickmdl <- function(series, spec, 
                        corona = FALSE, ..., 
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
                        add_comment = TRUE,
                        old_crit2 = NA) {
  
  original_option <- getOption("pickmdl.old_crit2")
  
  if (is.logical(corona)) {
    if (corona) {
      corona <- "ssb"
    } else {
      corona <- NULL
    }
  }
  
  if(!(output %in% c("sa", "spec", "sa_spec", "all")))
    stop('Allowed values of parameter output are "sa", "spec", "sa_spec" and "all".')
  
  automdl.enabled <- isTRUE(automdl.enabled)
  
  auto_in_pickmdl <- FALSE
  
  if (!all(apply(sapply(spec, class), 1, unique) == c("SA_spec", "X13"))) {
    if (!all(class(spec) == c("SA_spec", "X13"))) {
      stop("Wrong `spec` input")
    }
    if (automdl.enabled) {
      spec <- list(spec)
    } else {
      if(pickmdl_method %in% c("first_automdl", "first_tryautomdl")){
        spec <- c(x13_spec_pickmdl(spec), list(spec))
        spec[[length(spec)]] <- x13_spec(spec[[length(spec)]], automdl.enabled = TRUE)
        if(pickmdl_method=="first_automdl"){
          star <- length(spec)
        }
        auto_in_pickmdl <- TRUE
        pickmdl_method <- "first"
      } else {
        spec <- x13_spec_pickmdl(spec)
      }
    }
  }
  
  if (!is.null(corona) | is.na(old_crit2)) { 
    end_ts <- end(ts(1:2, start = end(window(series, end = identification_end)), frequency = frequency(series)))
    end_ts_final <- end(ts(1:2, start = end(series), frequency = frequency(series)))
    if (frequency(series) == 4) {
      end_ts[2] <- 1 + (end_ts[2] - 1) * 3
      end_ts_final[2] <- 1 + (end_ts_final[2] - 1) * 3
    }
    outlier_date_limit <- paste(end_ts[1], Number(end_ts[2], 2), "01", sep = "-")
    outlier_date_limit_final <- paste(end_ts_final[1], Number(end_ts_final[2], 2), "01", sep = "-")
    if (!is.null(identification_estimate.to)) {
      outlier_date_limit <- identification_estimate.to
    }
  }
  
  if (is.na(old_crit2)) {
    threshold_date <- "2024-10-15"
    old_crit2 <- as.Date(outlier_date_limit) < as.Date(threshold_date)
    if (verbose) {
      cat("old_crit2 set to ", old_crit2, " based on the date found (", outlier_date_limit, 
          ") and threshold (", threshold_date, ").\n", sep = "")
    }
  }
  
  
  # Set the option to `old_crit2` only if it does not already exist
  if (is.null(original_option)) {
    options(pickmdl.old_crit2 = old_crit2)
    
    # Ensure that the option is removed when the function exits
    on.exit(options(pickmdl.old_crit2 = NULL), add = TRUE)
  }
  
  if (!is.null(corona)) {  # Because of possible error (bug) only include outliers within estimation span 
    for (i in seq_along(spec)) {
      spec[[i]] <- update_spec_corona_outliers(spec[[i]], option = corona, outlier_date_limit = outlier_date_limit, freq = frequency(series))
    }
  }
  
  if (automdl.enabled) {
    spec <- spec[1]
    spec[[1]] <- x13_spec(spec[[1]], automdl.enabled = TRUE)
  }
  
  if (fastfirst) {
    fastfirst <- !automdl.enabled & pickmdl_method == "first"
  }
  
  if (fastfirst) {
    sa_mult <- NULL
    crit_tab <- NULL
    ok_loop <- FALSE
    ok <- TRUE
    i <- 0
    when_star_here <- NULL
    while (!ok_loop) {
      i <- i + 1
      # almost same code as below (spec -> spec[i])
      if (is.null(identification_estimate.to)) {
        sa_mult <- c(sa_mult, x13_multi(series = window(series, end = identification_end), spec = spec[i], ...))
      } else {
        sa_mult <- c(sa_mult, x13_multi(series = window(series, end = identification_end), 
                                        spec = lapply(spec[i], x13_spec, estimate.to = identification_estimate.to), ...))
      }
      crit_tab_i <- crit_table(sa_mult[i])
      if (i == length(spec)) {
        when_star_here <- when_star
      }
      ok_loop <- as.logical(crit_selection(crit_tab_i, star = 0, when_star = when_star_here))
      crit_tab <- rbind(crit_tab, crit_tab_i)
      if (ok_loop) {
        mdl_nr <- i
      }
      if (!ok_loop & i == length(spec)) {
        mdl_nr <- star
        ok_loop <- TRUE
        ok <- FALSE
      }
    }
  } else {
    if (is.null(identification_estimate.to)) {
      sa_mult <- x13_multi(series = window(series, end = identification_end), spec = spec, ...)
    } else {
      sa_mult <- x13_multi(series = window(series, end = identification_end), 
                           spec = lapply(spec, x13_spec, estimate.to = identification_estimate.to), ...)
    }
    
    if (automdl.enabled) {
      crit_tab <- NULL
      mdl_nr <- 1L
      ok <- crit_ok(sa_mult[[mdl_nr]])
    } else {
      crit_tab <- crit_table(sa_mult)
      mdl_nr <- crit_selection(crit_tab, pickmdl_method = pickmdl_method, star = star, when_star = when_star)
      if (!mdl_nr) {
        mdl_nr <- star
        ok <- FALSE
      } else {
        ok <- TRUE
      }
    }
  }
  
  
  
  
  
  if(verbose){
    print(sa_mult[[mdl_nr]]$regarima$arma)
  }
  
  length_spec <- length(spec)
  spec <- spec[[mdl_nr]]
  
  if(automdl.enabled | (auto_in_pickmdl & mdl_nr == length_spec)){
    if(!automdl.enabled){
      if(!is.null(when_automdl)){
        when_automdl("automdl since no pickmdl model ok")
      }
    }
    arma <- sa_mult[[mdl_nr]]$regarima$arma  # as.numeric remove names, as.numeric needed? can be factors?  
    spec <- x13_spec(spec, 
                     arima.p = as.numeric(arma["p"]), 
                     arima.d = as.numeric(arma["d"]), 
                     arima.q = as.numeric(arma["q"]), 
                     arima.bp = as.numeric(arma["bp"]), 
                     arima.bd = as.numeric(arma["bd"]), 
                     arima.bq = as.numeric(arma["bq"]),
                     automdl.enabled = FALSE)
  }
  
  if (identify_arima_mu) {
    spec <- x13_spec(spec, arima.mu = arima_mu(sa_mult[[mdl_nr]]))
  }
  
  if (identify_t_filter | identify_s_filter) {
    filters <- filter_input(sa_mult[[mdl_nr]])
    if (identify_t_filter) {
      spec <- x13_spec(spec, x11.trendAuto = FALSE, x11.trendma = filters[["x11.trendma"]])
    }
    if (identify_s_filter) {
      spec <- x13_spec(spec, x11.seasonalma = filters[["x11.seasonalma"]])
    }
    if (verbose) {
      print(unlist(filters)[c(identify_t_filter, identify_s_filter)], quote = FALSE)
    }
  }
  
  
  if (!is.null(corona)) { # Because of new final limit possible extra outliers included  
    spec <- update_spec_corona_outliers(spec, option = corona, outlier_date_limit = outlier_date_limit_final, freq = frequency(series))
  }
  
  if (identify_outliers) {
      spec <- update_spec_outliers(sa = sa_mult[[mdl_nr]], spec = spec, verbose = verbose)
  }
  
  if(output == "spec"){
    return(spec)
  }
  
  sa <- x13(series = series, spec = spec, ...)
  
  # Include possibility to check differences.
  # Seen that !isTRUE(all_equal) happen as result of specified outlier at end of series.
  # End outlier not included in first model, but included after outlier.from updated. 
  if (is.null(identification_end) & is.null(identification_estimate.to)) {
    if (get0("check_all.equal", ifnotfound = FALSE)) {
      all_equal <- all.equal(sa$final$series, sa_mult[[mdl_nr]]$final$series)
      if (isTRUE(all_equal))
        message(all_equal) else warning(all_equal)
    }
  }
  
  ok_final <- crit_ok(sa)
  
  if (!is.null(when_finalnotok)) {
    if (!ok_final) {
      when_finalnotok("FINAL RUN NOT OK")
    }
  }
  
  if (add_comment) {
    comment(sa) <- c(ok = as.character(ok), ok_final = as.character(ok_final), mdl_nr = as.character(mdl_nr))
  }
  
  if(output == "sa_spec"){
    return(list(sa = sa, spec = spec))
  }
  
  if(output == "all"){
    return(list(sa = sa, spec = spec, mdl_nr = mdl_nr, crit_tab = crit_tab, sa_mult = sa_mult))
  }
  
  sa
}

#' @rdname x13_pickmdl
#' @export
x13_automdl <- function(..., automdl.enabled = TRUE){
  x13_pickmdl(..., automdl.enabled = automdl.enabled)
}


