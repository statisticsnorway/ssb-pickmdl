
# Had to be included in helper file to work

# Trick to avoid testthat-bug: helper file sourced twice
# See https://stackoverflow.com/questions/47350561/why-is-helper-file-in-testthat-sourced-twice
# But this is not solved ... 
if(!get0("helper_sourced", ifnotfound = FALSE)){
  
myseries <- pickmdl_data("myseries")
seriesABC <- cbind(A = myseries, B = myseries + 10, C = myseries + 20)

tf <- data.frame(name = c("A", "B", "C"), automdl.enabled = c("TRUE", "FALSE", "FALSE"),
                 usrdef.outliersDate = c('c("2009-01-01", "2016-01-01")', 'c("2009-01-01")', NA),
                 usrdef.outliersType = c('rep("LS", 2)', '"AO"', NA),
                 usrdef.outliersEnabled = c("TRUE", "TRUE", NA))

x13_both_old_method <- FALSE   

outABC <- x13_text_frame(tf, series = "seriesABC", spec = "RSA3", transform.function = "Log")
outB   <- x13_text_frame(tf, series = "seriesABC", spec = "RSA3", transform.function = "Log", 
                         id = "B")

x13_both_old_method <- TRUE

outABC_old <- x13_text_frame(tf, series = "seriesABC", spec = "RSA3", transform.function = "Log")
outB_old   <- x13_text_frame(tf, series = "seriesABC", spec = "RSA3", transform.function = "Log", 
                         id = "B")


helper_sourced = TRUE 

}

