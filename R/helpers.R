
#' cintervalplot
#' 
#' Just a helpful wrapper that demonstrates how to make a confidence-interval
#' plot from raw probability-outcome data.
#'
#' @param pred Vector of predictions
#' @param actual Vector of outcomes 
#' @param ... Parameters passed on to \code{bootstrappedROC}
#' @return A ggplot object
#' @examples
#' testdata <- examplePredictions() 
#' cintervalplot(testdata$pred, testdata$actual)
#'
#' @export

cintervalplot <- function(pred, actual, ...){
   res <- bootstrappedROC(tdata$pred, tdata$actual)

   stacked <- data.frame(fallout = c(res$roc$fallout_025,rev(res$roc$fallout_975)),
                         recall = c(res$roc$recall_975,rev(res$roc$recall_025)))

   ggplot(res$roc,aes (x = fallout_mean, y = recall_mean))+
      geom_line() +
      geom_polygon(aes(x = fallout,y = recall), data = stacked, alpha = 0.2)
}
