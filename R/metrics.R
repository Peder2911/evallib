
library(dplyr)
library(zoo)

#' bootstrap

#' confmat
#' 
#' Returns a 2x2 matrix with the results of a classification procedure. Useful
#' for computing further statistics.
#'
#' @param predicted Predicted values 
#' @param actual Actual values 
#' @return A 2x2 matrix with TN,FN,FP,TP
#' @examples
#' confmat(c(1,1,0),c(0,1,0))
#'
#'
#' @export
confmat <- function(predicted,actual){
   mat <- matrix(nrow = 2, ncol = 2)
   mat[1,1] <- sum(predicted == 0 & actual == 0)
   mat[1,2] <- sum(predicted == 1 & actual == 0)
   mat[2,1] <- sum(predicted == 0 & actual == 1)
   mat[2,2] <- sum(predicted == 1 & actual == 1)
   mat
}

#' withConfmat
#' 
#' Helper function. Computes a statistic from predicted and actual values,
#' using an intermediate confusion matrix.
#'
#' @param p Predicted values
#' @param a Actual values
#' @return The result of whatever function is passed as fun (recall/precision/accuracy etc.)
#' @examples
#' battery <- list(precision = precision, recall = recall,
#'                 fallout = fallout, accuracy = accuracy) 
#' pred <- sample(c(1,0),replace = TRUE, size = 150)
#' actual <- sample(c(1,0),replace = TRUE, size = 150)
#' lapply(battery,function(fun){
#'        withConfmat(pred,actual,fun)})
#' @export
withConfmat <- function(p,a,fun){
   fun(confmat(p,a))
}

#' zerosafeDiv
#'
#' Division, with the exception that division by zero yields one.
#'
#' @param x A number
#' @param y A number
#'
#' @return A number
#'
#' @examples
#' zerosafeDiv(1,0) 
#' [1] 1
#' zerosafeDiv(1,1) 
#' [1] 1
zerosafeDiv <- function(x,y){
   if(y == 0){
      1
   } else {
      x / y
   }
}

#' recall
#' 
#'
#' @param confmat A confusion matrix
#' @return The recall value of the predicted and actual values
#' @export
recall <- function(confmat){
   zerosafeDiv(confmat[2,2] , sum(confmat[2,]) )
}

#' precision 
#' 
#'
#' @param confmat A confusion matrix
#' @return The precision value of the predicted and actual values
#' @export
precision <- function(confmat){
   zerosafeDiv(confmat[2,2] , sum(confmat[,2]))
}

#' fallout 
#' 
#'
#' @param confmat A confusion matrix
#' @return The fallout value of the predicted and actual values
#' @export
fallout <- function(confmat){
   zerosafeDiv(confmat[1,2] , sum(confmat[1,]))
}

#' accuracy 
#' 
#'
#' @param confmat A confusion matrix
#' @return The accuracy value of the predicted and actual values
#' @export
accuracy <- function(confmat){
   zerosafeDiv((confmat[1,1] + confmat[2,2]) , sum(confmat))
}

#' fscore
#' 
#' The F-score is computed from precision and recall, representing a balance
#' between the two statistics.  
#'
#' @param precision Precision statistic
#' @param recall Recall statistic
#' @return F-statistic
fscore <- function(precision,recall,beta = 1){
   (1+ (beta^2)) * ((precision * recall)/(((beta^2) * precision) + (recall)))
}

#' categorizeClasf
#' 
#' Just names a combination of predicted / actual values as either TP, FP, TN, FN.
#'
#' @param pred Predicted values 
#' @param pred Actual values 
#' @return A character vector
#' @export
categorizeClasf <- function(pred,act){
   sapply(1:length(pred), function(i){
      predicted <- pred[i]
      actual <- act[i]
      if(predicted == 1 & actual == 1){
         'TP'
      } else if(predicted == 1 & actual == 0){
         'FP'
      } else if(predicted == 0 & actual == 0){
         'TN'
      } else if(predicted == 0 & actual == 1){
         'FN'
      }
   })
}

#' auc
#' 
#' Returns the Area Under Curve statistic, which is often used to compare
#' \code{metricCurve}s. 
#'
#' @param x The X dimension of the metric curve.
#' @param y The Y dimension of the metric curve.
#' @return An AUC score
#' @examples
#' predicted <- sample(c(1,0), replace = TRUE, size = 150)
#' actual <- sample(c(1,0), replace = TRUE, size = 150)
#' myRoc <- roc(predicted,actual)
#' auc(myRoc$fallout, myRoc$recall)
#' @export
auc <- function(x,y){
   # ? 
   sum(diff(x) * zoo::rollmean(y,2))
}

#' metricCurve
#' 
#' Creates a Metric Curve dataset, that can be used to create a metric plot,
#' like a ROC plot or a Precision-recall plot, and calculate AUC. ROC is a
#' special, widely used case of the MC.
#'
#' @param p Vector of predicted probabilities
#' @param actual Vector of actual scores
#' @param x A function operating on the 2x2 confusion matrix, yielding a score.
#' Common functions are precision, recall, fallout and accuracy.
#' @param y A function operating on the 2x2 confusion matrix, yielding a score.
#' Common functions are precision, recall, fallout and accuracy.
#' @param res Specification of the threshold resolution of the curve. Either a
#' single number, a vector of numbers, or NULL. If a single number, it will be
#' used as an interval to create thresholds between 0 and 1. If a vector, it
#' will be used  as the vector of thresholds. If NULL, thresholds will be based
#' on the unique values of p.
#' @return A data-frame of metric scores, that can be used to create an
#' interesting plot, or to calculate the AUC.
#' @examples
#' predicted <- sample(seq(0,1,0.01),replace = TRUE, size = 150)
#' actual <- sample(c(1,0),replace = TRUE, size = 150)
#' metricCurve(predicted, actual, precision, recall)
#' @export
#' @importFrom magrittr "%>%"

metricCurve <- function(p,actual, x, y, res = NULL){
   # Figuring out the thresholds to create the ROC curve with:
   xFnName <- as.character(substitute(x))
   yFnName <- as.character(substitute(y))

   if(is.null(res)){
      # Uses the unique values of p as thresholds 
      # Creates a thresh. between each unique value
      uniquePredictions <- sort(unique(p), decreasing = TRUE)
      breaks <- (c(1,uniquePredictions) + c(uniquePredictions,0)) / 2
   } else if(length(res) == 1) {
      # Creates a vector with steps of res 
      breaks <- seq(1,0,-res)
   } else {
      # Uses the vector res 
      breaks <- res
   }


   lapply(breaks, function(threshold){
      predicted <- as.numeric(p > threshold)
      res <- list(th = threshold)
      res[[xFnName]] = withConfmat(predicted,actual,x)
      res[[yFnName]] = withConfmat(predicted,actual,y)
      res
   }) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(-th)
}

#' roc
#' 
#' A special case of the \code{metricCurve} function, which is widely used to
#' diagnose classifiers. A ROC is simply a curve formed by calculating the
#' metrics \code{fallout} and \code{recall} for classification values and
#' actual cases, using different thresholds for the classification values.
#' 
#' For implementation, see \code{metricCurve}. 
#' @param p Vector of predicted probabilities
#' @param actual Vector of actual scores
#' @param res Specification of the threshold resolution of the curve. Either a
#' single number, a vector of numbers, or NULL. If a single number, it will be
#' used as an interval to create thresholds between 0 and 1. If a vector, it
#' will be used  as the vector of thresholds. If NULL, thresholds will be based
#' on the unique values of p.
#' @return A data-frame of ROC scores.
#' @examples
#' predicted <- sample(seq(0,1,0.01),replace = TRUE, size = 150)
#' actual <- sample(c(1,0),replace = TRUE, size = 150)
#' metricCurve(predicted, actual, precision, recall)
#' @export
#' @importFrom magrittr "%>%"
#' 
#' @export
#' @importFrom magrittr "%>%"
roc <- function(p,actual,res = NULL){
   metricCurve(p,actual,x = fallout, y = recall, res = res)
}

# Old ROC function.
#roc <- function(p,actual,res = NULL){
#
#   # Figuring out the thresholds to create the ROC curve with:
#   if(is.null(res)){
#      # Uses the unique values of p as thresholds 
#      # Creates a thresh. between each unique value
#      uniquePredictions <- sort(unique(p), decreasing = TRUE)
#      breaks <- (c(1,uniquePredictions) + c(uniquePredictions,0)) / 2
#   } else if(length(res) == 1) {
#      # Creates a vector with steps of res 
#      breaks <- seq(1,0,-res)
#   } else {
#      # Uses the vector res 
#      breaks <- res
#   }
#
#   lapply(breaks, function(threshold){
#      predicted <- as.numeric(p > threshold)
#      list(fallout = withConfmat(predicted,actual,fallout),
#           recall = withConfmat(predicted,actual,recall),
#           th = threshold)
#   }) %>%
#      dplyr::bind_rows() %>%
#      dplyr::arrange(-th)
#}

#' aucFromPA
#' 
#' Just a helper function that yields an AUC score from predicted and actual values. 
#'
#' @param p Vector of predicted probabilities
#' @param actual Vector of actual scores
#' @return An AUC score
#' @examples
#' predicted <- sample(seq(0,1,0.01),replace = TRUE, size = 150)
#' actual <- sample(c(1,0),replace = TRUE, size = 150)
#' aucFromPA(predicted,actual)
#' @export
aucFromPA <- function(p,actual, ...){
   roc <- roc(p,actual, ...)
   auc(roc$fallout, roc$recall)
}

# ================================================
# Just trash
# ================================================

either <- function(data,a,b){
   as.numeric(data[[as.character(substitute(a))]] | 
              data[[as.character(substitute(b))]])
}

#' ???
addConf <- function(data,predicted,actual){
   res <- apply(data,1,function(row){
      mat <- confmat(as.numeric(row[as.character(quote(predicted))]),
                     as.numeric(row[as.character(quote(actual))]))
      list(tn = mat[1,1],fp= mat[1,2],fn = mat[2,1], tp = mat[2,2])
   })
   cbind(data,do.call(rbind,res))
}
