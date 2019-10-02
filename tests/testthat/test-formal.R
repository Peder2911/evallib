
library(glue)

test_that("Formal tests", {
   iterateOverTests <- function(cases,outcomes,fun, wrapper = NULL){

      lapply(1:length(outcomes), function(test){
         c <- cases[[test]]

         if(!is.null(wrapper)){
         c[[3]] <- fun
         } else {
            wrapper <- fun
         }
         expect_equal(do.call(wrapper,c),outcomes[test],
                      info = glue("test number {test}"))
   })
   }

   cases <- list(allcorrect = list(c(1,0),c(1,0)),
                 missedone = list(c(0,0),c(1,0)),
                 optimistic = list(c(1,1),c(1,0)),
                 pessimistic = list(c(0,1),c(1,1)))

   # Recall is the rate of True Positives out of Positives.
   outcomes <- c(1,
                 0,
                 1,
                 0.5)
   iterateOverTests(cases,outcomes,recall, wrapper = withConfmat)

   # Precision is the rate of True Positives out of all Predicted Positives
   # precision is 1 with no predicted positives
   outcomes <- c(1,
                 1,
                 0.5,
                 1)
   iterateOverTests(cases,outcomes,precision, wrapper = withConfmat)

   # Accuracy is the rate of True Predictions out of All Values
   outcomes <- c(1,
                 0.5,
                 0.5,
                 0.5)
   iterateOverTests(cases,outcomes,accuracy, wrapper = withConfmat)

   # Fallout is the rate of False Positives out of all Negatives 
   # fallout is 1 with no actual negatives
   outcomes <- c(0,
                 0,
                 1,
                 1)
   iterateOverTests(cases,outcomes,fallout, wrapper = withConfmat)
})
