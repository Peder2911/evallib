
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

cintervalplot <- function(pred, actual, res = 0.1, parallel = TRUE){
   res <- bootstrappedROC(pred, actual, res = res, parallel = parallel)
   res$roc <- res$roc[order(res$roc$th),]
   
   points <- roc(pred,actual,c(0,0.3,0.5,1))
   points <- points[c(2,3),]

   stacked <- data.frame(fallout = c(res$roc$fallout_025,rev(res$roc$fallout_975)),
                         recall = c(res$roc$recall_975,rev(res$roc$recall_025)))

   list(plot = ggplot2::ggplot(res$roc,ggplot2::aes(x = fallout_mean, y = recall_mean))+
               ggplot2::geom_path() +
               ggplot2::geom_polygon(ggplot2::aes(x = fallout,y = recall), data = stacked, alpha = 0.2) +
               ggplot2::geom_point(ggplot2::aes(x =fallout, y = recall), data = points, size = 2, color = "red") +
               ggplot2::geom_text(ggplot2::aes(x =fallout, y = recall, label = as.character(th)), data = points, 
                                  nudge_x = 0.05, nudge_y = -0.03, size = 5, color = "red"),
        results = res)
}

#' aucWithCI 
#'
#' Wrapper for aucFromPA that also calls \code{pROC::ci.auc} to obtain DeLong
#' confidence intervals.
#'
#' @param pred Vector of predictions
#' @param actual Vector of outcomes 
#' @return A list with score and quantiles 
#' @examples
#' testdata <- examplePredictions() 
#' aucWithCI(testdata$pred, testdata$actual) 
#'
#' @export
aucWithCI <- function(pred,actual){
   if(requireNamespace("pROC",quietly = TRUE)){
      ci <- pROC::ci.auc(actual,pred)
      list(score = aucFromPA(pred,actual),
          quantiles = c(ci[1],ci[3]))
   } else {
      stop("This function requires the package pROC")
   }
}
