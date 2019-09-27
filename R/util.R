
#' examplePredictions 
#' 
#' Generates two vectors, one with predictions and one with outcomes.  Used to
#' test functions. Stochastic, seed must be set to get reproducible results. 
#'
#' @return A data.frame with $pred and $actual columns.
#' @examples
#' set.seed(1337)
#' tdat <- examplePredictions()
#' bootstrap(tdat$pred, tdat$actual, roc)
#' @export

examplePredictions <- function(n = 150, noise = 0.2){

   normalize <- function(x){
      nozero <- (x + abs(min(x))) 
      nozero / max(nozero)
   }

   poisprobs <- function(n, res){
      range <- seq(0,1,res)
      probs <- rpois(n = length(range),lambda = 1)
      probs <- probs / max(probs)

      sample(range, replace = TRUE, size = n, prob = sort(probs))
   }

   actual <- sample(c(1,0),replace = TRUE, size = n)
   mode <- (actual * -2) + 1

   # Poisson probabilities based on outcome
   pred <- 1-(actual + (poisprobs(n, 0.001) * mode))
   pred <- normalize(pred + runif(n = n, max = noise, min = -noise))

   data.frame(pred=pred,actual=actual)
}


