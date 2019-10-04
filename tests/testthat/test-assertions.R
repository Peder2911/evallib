
test_that("assertions",{
   n <- 0
   for(i in 1:10){
      n <- n + 0.1
      data <- examplePredictions(noise = n)
      expect_error(predAssertions(data$pred), NA)
      expect_error(actAssertions(data$actual), NA)
   }
   expect_error(predAssertions(c(1,0,-1)),"less than")
   expect_error(predAssertions(c(3,0,1)),"more than")
   expect_error(predAssertions(c("a","b")),"not numeric")

   expect_error(actAssertions(c("a","b")),"not numeric")
   expect_error(actAssertions(c(0,1,2,0,0,1)),"too many")
   expect_error(actAssertions(c(2,1,2,2,1,2)),"values 0 and 1")
})

