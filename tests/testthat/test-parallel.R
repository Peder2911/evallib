test_that("Parallel bootstrap", {
  tdata <- evallib::examplePredictions(n = 100)
  expect_error(
     res <- evallib::bootstrappedROC(tdata$pred,tdata$actual,parallel = TRUE,n = 200),
     NA
  )
})
