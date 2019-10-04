test_that("Parallel bootstrap", {
  tdata <- evallib::examplePredictions(n = 1000)

  expect_error(
     res <- evallib::bootstrappedROC(tdata$pred,tdata$actual, res = 0.01,parallel = TRUE),
     NA
  )
})

test_that("Bootstrap", {
  tdata <- evallib::examplePredictions(n = 100)

  expect_error(evallib::bootstrappedROC(tdata$pred,tdata$actual, res = 0.01),NA)
})
