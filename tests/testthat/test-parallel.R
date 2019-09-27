test_that("Parallel bootstrap", {
  tdata <- evallib::examplePredictions(n = 10000)
  expect_error(
     res <- evallib::bootstrappedROC(tdata$pred,tdata$actual,parallel = TRUE,n = 2000),
     NA
  )
})
