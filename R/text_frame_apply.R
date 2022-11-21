



#' Apply function(s) with input from a data frame 
#' 
#' 
#' @details 
#' Data frame where all variables are character and with parameter names as column names. 
#' Each cell contains text with R code written as source code in a function call.
#' The parameter will be omitted when the cell is missing (`NA`).  
#' The row names will be used as names in the output and can be used in selections with the `id` parameter.
#' With `fun = NULL`, the first column must contain function name(s) to be called. 
#'
#' @param text_frame Data frame to specify function arguments. See details. 
#' @param fun fun 
#' @param id id 
#' @param ... dots
#' @param call_list calll
#' @param drop drop
#' @param verbose verb
#' @param envir envir
#' 
#' @note This function is general and may be usable outside the pickmdl package.
#'
#' @return
#' @export
#'
#' @examples
#' ax_plus_b <- function(a = 2, b = 3, x = 5) {a * x + b}
#' z <- data.frame(a = c("1", "2", NA), b = "7", x = c(NA, "9", "2"))
#' rownames(z) <- c("A", "B", "C")
#' z
#' text_frame_apply(z, "ax_plus_b", verbose = TRUE)
#' text_frame_apply(cbind(data.frame("ax_plus_b"), z))
#' text_frame_apply(z[c(1, 3)], "ax_plus_b", id = 1:2)
#' text_frame_apply(z[c(1, 3)], "ax_plus_b", b = 7, id = "B")
#' text_frame_apply(z[c(1, 3)], "ax_plus_b", call_list = list(b = 7))
#' text_frame_apply(z[c(1, 3)], "ax_plus_b", b = 1:2, id = "B", drop = FALSE)
#' text_frame_apply(z[3], "ax_plus_b", a = 1, call_list = list(b = 7))
text_frame_apply <- function(text_frame, fun = NULL, id = NULL, ..., drop = TRUE, verbose = FALSE, envir = NULL, call_list = NULL) {
  if (is.null(envir)) {
    envir <- parent.frame()
  }
  if (is.null(id)) {
    drop <- FALSE
  } else {
    text_frame <- text_frame[id, , drop = FALSE]
  }
  out <- vector("list", nrow(text_frame))
  names(out) <- rownames(text_frame)
  for (i in seq_len(nrow(text_frame))) {
    if (verbose) {
      cat("  ----   id = ", rownames(text_frame)[i], "   ----\n")
    }
    line_i <- make_line_i(text_frame = text_frame, i = i, fun = fun)
    call_i <- as.call(c(line_i, list(...), call_list))
    if (verbose) {
      print(call_i)
      cat("\n")
      flush.console()
    }
    out[[i]] <- eval(call_i, envir = envir)
  }
  
  if (length(out) == 1 & drop == TRUE) {
    return(out[[1]])
  }
  
  out
}


make_line_i <- function(text_frame, i = 1, verbose = FALSE, fun = NULL) {
  text_frame_i <- as.list(text_frame[i, , drop = FALSE])
  text_frame_i <- text_frame_i[!sapply(text_frame_i, is.na)]  # missing elements removed 
  
  if (!is.null(fun)) {
    if (is.character(fun)) {
      text_frame_i <- c(list(fun), text_frame_i)
    } else {
      stop("fun must be character")
    }
  }
  line_i <- eval(parse(text = paste("alist(", paste(text_frame_i, collapse = ", "), ")", collapse = "")))
  names(line_i) <- names(text_frame_i)
  names(line_i)[1] <- ""
  line_i
}







