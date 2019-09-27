
#' bootstrap
#' 
#' Apply \code{fun} to the data \code{n} times.  The data is randomly sampled
#' each time. This yields both more robust results and approximate confidence
#' intervals for the results.  Due to the random sampling, the results of this
#' function are stochastic. Set the seed with \code{set.seed(n)} for
#' reproducible results. 
#'
#' @param 
#' @return 
#' @examples
#' pred <- sample(seq(0,1,0.001), size = 150, replace = TRUE)
#' actual <- sample(c(0,1), size = 150, replace = TRUE)
#' bootstrap(pred,actual,roc)
#'
#' @export
bootstrap <- function(pred, actual, fun, ratio = 0.5, draws = 100, ...){
   if(ratio>1 | ratio<0) stop(glue::glue("Ratio must be 0-1, not {ratio}"))
   if(length(pred) != length(actual)) stop("pred and actual must have the same length!")

   data <- data.frame(pred = pred,actual = actual)

   lapply(draws, function(sample){
      # Draw the sample for this
      draw <- sample(c(TRUE,FALSE),size = nrow(data),replace = TRUE,
                     prob = c(ratio,1-ratio))
      data[draw,]
      fun(data$pred,data$actual, ...)
   })
}
