test_that("x13_pickmdl works ok", {
  myseries <- pickmdl_data("myseries")
  
  spec_c <- x13_spec(outlier.enabled = FALSE) 
  
  
  q <- c(2, 3, 11, 45, 29, 11, 6, 4, 4, 2, 3, 9, 8, 3, 1, 2, 25, 19, 
         11, 125, 6, 7, 10, 7, 31, 49, 28, 4, 5, 4, 3, 7, 18, 17, 33, 
         1, 5, 3, 10, 43, 35, 21, 1, 0, 3, 2, 8, 15, 11, 4, 1, 3, 1, 1, 
         4, 3, 6, 21, 20, 18, 58, 23, 18, 1, 2, 2, 2, 14, 18, 48, 10, 
         1, 1, 9, 254, 319, 201, 14, 7, 3, 8, 3, 4, 2, 4, 11, 17, 9, 6, 
         1, 2, 1, 3, 46, 90, 197, 160, 21, 11, 1, 5, 4, 24, 3, 15, 7, 
         38, 24, 9, 11, 8, 5, 3, 52, 14, 25, 11, 3, 1, 1, 1, 1, 7, 13, 
         135, 101, 163, 52, 35, 20, 8, 17, 13, 35, 9, 22, 10, 4, 1, 10, 
         25, 29, 38, 17, 3, 0, 1, 3, 4, 13, 11, 25, 62, 16, 4, 4, 24, 
         4, 3, 2, 6, 116, 46, 16, 1, 1, 4, 5, 8, 7, 8, 43, 99, 319, 10, 
         3, 7, 7, 16, 8, 1, 1, 14, 18, 146, 122, 65, 84, 4, 2, 2, 2, 4, 
         1, 1, 5, 13, 77, 30, 10, 4)
  
  
  
  d1 <- x13_pickmdl(myseries, spec_c, pickmdl_method = "first_automdl", when_finalnotok = warning)
  d2 <- x13_pickmdl(myseries*(1+ 0.3*q), spec_c, pickmdl_method = "first_automdl", when_finalnotok = warning)
  d3 <- x13_pickmdl(myseries*(1+ 0.15*q), spec_c, pickmdl_method = "first_automdl", when_finalnotok = warning)
  d4 <- x13_pickmdl(myseries*(1+ 0.1*q), spec_c, pickmdl_method = "first_automdl", when_finalnotok = warning)
  expect_warning({d4b <-x13_pickmdl(myseries*(1+ 0.1*q), spec_c, pickmdl_method = "first_automdl", when_finalnotok = warning, identify_arima_mu = FALSE)})
  d5 <- x13_pickmdl(myseries*(1+ 0.25*q)+10, spec_c, pickmdl_method = "first_automdl", when_finalnotok = warning)
  expect_warning({d6 <- x13_pickmdl(myseries+(q)^2, spec_c, pickmdl_method = "first_tryautomdl", when_finalnotok = message)})
  
  expect_equal(d1$regarima$arma, c(0, 1, 2, 0, 1, 1), ignore_attr = TRUE)
  expect_equal(d2$regarima$arma, c(2, 1, 2, 0, 1, 1), ignore_attr = TRUE)
  expect_equal(d3$regarima$arma, c(2, 0, 1, 1, 0, 1), ignore_attr = TRUE)
  expect_equal(d4$regarima$arma, c(2, 0, 1, 1, 0, 1), ignore_attr = TRUE)
  expect_equal(d5$regarima$arma, c(0, 1, 1, 0, 1, 1), ignore_attr = TRUE)
  expect_equal(d6$regarima$arma, c(0, 1, 1, 0, 1, 1), ignore_attr = TRUE)
  
})