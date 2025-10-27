



#' Multiple \code{\link{x13_both}} runs with code input from a data frame
#'
#' @param text_frame Data frame where all variables are character and with parameter names as column names.
#'                   Each cell contains text with R code written as source code in a call to \code{\link{x13_both}}. 
#'                   The parameter will be omitted when the cell is missing (NA). 
#'                   The exception is the column name, `name`, which contains the time series names. 
#'                   Without such a column, the names are taken from the row names. 
#' @param series A named multiple time series object, given as a character string.
#'               When `NULL`, the series parameter must be included in `text_frame`.  
#' @param id To select specific time series to be processed (name or number).
#' @param ... Extra arguments that do not change.
#' @param drop Whether to omit list output when a single time series is specified by `id`.
#' @param verbose  When `TRUE`, function calls will be printed. 
#' @param dots2list A technical parameter.  When `TRUE` and when possible (warning when not), 
#'                  the underlying function, \code{\link{text_frame_apply}}, will be called via `call_list` instead of `...`. 
#'                  The advantage is prettier (unevaluated) printing when `verbose = TRUE`. 
#'
#' @return A list of `x13_both` outputs or output from a single run of `x13_both` (see `drop`).
#' @export
#'
#' @examples
#' myseries <- pickmdl_data("myseries")
#' seriesABC <- cbind(A = myseries, B = myseries + 10, C = myseries + 20)
#' 
#' tf <- data.frame(name = c("A", "B", "C"), automdl.enabled = c("TRUE", "FALSE", "FALSE"),
#'                  usrdef.outliersDate = c('c("2009-01-01", "2016-01-01")', 'c("2009-01-01")', NA),
#'                  usrdef.outliersType = c('rep("LS", 2)', '"AO"', NA),
#'                  usrdef.outliersEnabled = c("TRUE", "TRUE", NA))
#' 
#' outABC <- x13_text_frame(tf, series = "seriesABC", spec = "RSA3", transform.function = "Log", 
#'                          verbose = TRUE)
#' outB   <- x13_text_frame(tf, series = "seriesABC", spec = "RSA3", transform.function = "Log", 
#'                          id = "B")
#' identical(outABC[[2]], outB)  # TRUE
#' 
x13_text_frame <- function(text_frame, series = NULL, id = NULL, ..., drop = TRUE, verbose = FALSE, dots2list = TRUE) {
  text_frame <- character_frame(text_frame)
  if (dots2list) {
    sys_call <- sys.call()
    if (any(sapply(sys_call, function(x) identical(x, as.name("..."))))) {
      dots2list <- FALSE
      warning("dots2list ignored since ... used to call x13_text_frame")
    } else {
      fml_names <- names(formals(x13_text_frame))
      sys_call <- sys_call[!names(sys_call) %in% fml_names]
      sys_call <- as.list(sys_call)[-1]
      sys_call <- sys_call[names(sys_call) != ""]
    }
  }
  envir <- parent.frame()
  ma <- match("name", names(text_frame))
  if (!is.na(ma)) {
    rownames(text_frame) <- text_frame[[ma]]
    text_frame <- text_frame[-ma]
  }
  if (!is.null(series)) {
    if (is.character(series)) {
      text_frame$series <- paste0(series, "[,'", rownames(text_frame), "']")
    } else {
      stop("series must be character")
    }
  }
  if (dots2list) {
    return(text_frame_apply(text_frame = text_frame, fun = "x13_both", id = id, call_list = sys_call, drop = drop, verbose = verbose, envir = envir))
  }
  text_frame_apply(text_frame = text_frame, fun = "x13_both", id = id, ..., drop = drop, verbose = verbose, envir = envir)
}

# Modification (with warning and stop) of SSBtools::ForceCharacterDataFrame
character_frame <- function(x) {
  warn <- FALSE
  for (i in seq_len(ncol(x))) if (is.factor(x[, i, drop =TRUE])){
    warn <- TRUE
    x[, i] <- as.character(x[, i, drop =TRUE])
  } 
  if(warn){
    warning("text_frame factor variable(s) converted to character")
  }
  if(!all(sapply(x, class) == "character")){
    stop("text_frame variables must be character")
  }
  x
}
