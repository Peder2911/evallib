noisier <- function(...){
   lapply(seq(0.1,1,0.1), function(lvl) examplePredictions(noise = lvl, ...))
}

test_that("AUC decreases with", {
   data <- noisier() 

   AUC <- data %>%
      lapply(function(data){aucFromPA(data$pred,data$actual)}) %>%
      unlist()

   expect_lt(cov(1:10,AUC), 0)
})

test_that("Works like pROC", { 
   tolerance <- 0.001
   data <- noisier(n = 10000)
   lapply(data,function(dat){
      auc_evallib <- aucFromPA(dat$pred, dat$actual)
      auc_proc <- pROC::auc(dat$actual, dat$pred)
      diff <- abs(auc_evallib - auc_proc)
      expect_gt(tolerance, diff)
   })
   
})
