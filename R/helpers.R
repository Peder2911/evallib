
#' cintervalplot2
#' 
#' Just a helpful wrapper that demonstrates how to make a confidence-interval
#' plot from raw probability-outcome data, using metric functions x and y.
#'
#' @param pred Vector of predictions
#' @param actual Vector of outcomes 
#' @param x The X dimension of the metric curve.
#' @param y The Y dimension of the metric curve.
#' @param draws Number of times to randomly sample the data for bootstrapping.
#' Larger numbers yield better results, but also increase runtime. 
#' @param res Resolution of ROC thresholds. A float between 0 and 1. 
#' @param parallel Boolean. Run in parallel? Uses ncores - 1 CPU cores.
#' @return A list containing results and a ggplot object.
#' @examples
#' # To make a ROC plot with confidence intervals: 
#' testdata <- examplePredictions() 
#' cintervalplot(testdata$pred, testdata$actual, fallout, recall,
#'               draws = 1000, res = 0.01, parallel = TRUE)
#'
#' @export

cintervalplot2 <- function(pred, actual, x, y, 
                          draws = 100, res = 0.1, parallel = FALSE){
   xFnName <- as.character(substitute(x))
   yFnName <- as.character(substitute(y))

   res <- bootstrappedMetricCurve(pred, actual, x = x, y = y, 
                                  res = res, parallel = parallel, draws = draws)

   res$curve <- res$curve[order(res$curve$th),]
   
   points <- metricCurve(pred,actual,x=x,y=y,res = c(0,0.3,0.5,1))
   points <- points[c(2,3),]

   stacked <- data.frame(x = c(res$curve$x.q025,rev(res$curve$x.q975)),
                         y = c(res$curve$y.q975,rev(res$curve$y.q025)))
   

   list(plot = ggplot2::ggplot(res$curve,ggplot2::aes(x = x.mean, y = y.mean))+
               ggplot2::geom_path() +
               ggplot2::geom_polygon(ggplot2::aes(x = x,y = y), data = stacked, alpha = 0.2) +
               #ggplot2::geom_path(ggplot2::aes(x = x.q975, y = y.q025))+
               #ggplot2::geom_path(ggplot2::aes(x = x.q025, y = y.q975))+
               ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = points, 
                                   color = "red", size = 4) +
               ggplot2::geom_text(ggplot2::aes(x =x, y = y, label = as.character(th)), data = points, 
                                  nudge_x = 0.05, nudge_y = -0.03, size = 5, color = "red"),
        results = res,
        stacked = stacked)
}


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

cintervalplot <- function(pred, actual, x = fallout, y = recall, 
                          draws = 100, res = 0.1, parallel = TRUE){
   xFnName <- as.character(substitute(x))
   yFnName <- as.character(substitute(y))

   res <- bootstrappedROC(pred, actual, 
                          res = res, parallel = parallel, draws = draws)

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
